;;; orgwebalchemy

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; orgwebalchemy is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; orgwebalchemy is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the Lesser GNU General Public License
;; along with orgwebalchemy.  If not, see <https://www.gnu.org/licenses/>.

(define-module (orgwebalchemy org-to-html)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
  #:export (org-to-html))

;; =====================================================================
;; Org-mode PEG (Parsing Expression Grammar)
;; =====================================================================
;;
;; some tips for `define-peg-pattern'
;; 'none' means these match in the string but won't appear in the final AST.
;; 'body' means the node itself is omitted, but its matched children bubble up.
;; 'all' means both the tag and the matched string/children are kept in the AST.
;;

;; =====================================================================
;; Character & Whitespace Primitives
;; =====================================================================

(define-peg-pattern nl none
                    (or "\r\n" "\n"))
(define-peg-pattern ws none
                    (or " " "\t"))
(define-peg-pattern eof none
                    (not-followed-by peg-any))
(define-peg-pattern digit none
                    (range #\0 #\9))

;; =====================================================================
;; Core Document Structure
;; =====================================================================
(define-peg-pattern org-doc body
                    (* element))
(define-peg-pattern element body
                    (or empty-line
                        heading
                        src-block
                        unordered-item
                        ordered-item
                        paragraph))
(define-peg-pattern empty-line none
                    (and (* ws) nl))

;; =====================================================================
;; Block Level Elements
;; =====================================================================

;; Headings (*, **, ***)
(define-peg-pattern heading all
                    (and stars
                         (+ ws) line-content
                         (or nl eof)))
(define-peg-pattern stars all
                    (+ "*"))

;; =====================================================================
;; List Items & Description Terms
;; =====================================================================

(define-peg-pattern list-bullet none
                    (or "-" "+"))

(define-peg-pattern list-num none
                    (and (+ digit)
                         (or "." ")")))

(define-peg-pattern desc-sep none
                    (and (+ ws) "::"
                         (+ ws)))

(define-peg-pattern desc-key all
  (+ (and (not-followed-by (or nl desc-sep))
          peg-any)))

(define-peg-pattern desc-body body
  (and desc-key
       desc-sep
       line-content))

(define-peg-pattern item-content body
                    (or desc-body line-content))

;; Unordered List Items (- item or + item)
(define-peg-pattern unordered-item all
                    (and list-bullet
                         (+ ws) item-content
                         (or nl eof)))

;; Ordered List Items (1. item or 1) item)
(define-peg-pattern ordered-item all
                    (and list-num
                         (+ ws) item-content
                         (or nl eof)))

;; Source Blocks (#+BEGIN_SRC ... #+END_SRC)

(define-peg-pattern begin-src none
                    (or "#+BEGIN_SRC" "#+begin_src"))
(define-peg-pattern end-src none
                    (or "#+END_SRC" "#+end_src"))

(define-peg-pattern lang all
                    (* (and (not-followed-by nl) peg-any)))

(define-peg-pattern src-block all
                    (and begin-src
                         (+ ws)
                         lang
                         nl
                         block-content
                         end-src
                         (* ws)
                         (or nl eof)))

(define-peg-pattern block-content all
                    (* (and (not-followed-by end-src) peg-any)))

;; Paragraphs (Fallback: any line that doesn't start with a special character)
(define-peg-pattern special-starts none
                    (or "*" "-" "#+"
                        (and (* ws) nl)))
(define-peg-pattern paragraph all
                    (and (not-followed-by special-starts) line-content
                         (or nl eof)))

(define-peg-pattern line-content all
                    (* (and (not-followed-by (or nl eof)) peg-any)))

;; =====================================================================

;; =====================================================================
;; AST Transformation & Grouping
;; =====================================================================

(define (desc-item? node)
  "Return #t if NODE contains a 'desc-key child."
  (and (pair? node)
       (any (lambda (child)
              (and (pair? child)
                   (eq? (car child)
                        'desc-key)))
            (cdr node))))

(define (item-type node)
  "Determine the list grouping type for NODE."
  (cond
    ((not (pair? node))
     #f)

    ((desc-item? node)
     'description-list)

    ((eq? (car node)
          'unordered-item)
     'unordered-list)

    ((eq? (car node)
          'ordered-item)
     'ordered-list)

    (else #f)))

(define (group-list-items ast)
  "Group consecutive list items into unordered-list, ordered-list, or description-list containers."
  (let loop
    ((rest-nodes ast)
     (current-list '())
     (current-type #f)
     (acc '()))
    (cond
      ;; Reached end of document
      ((null? rest-nodes)
       (reverse (if (null? current-list) acc
                    (cons `(,current-type ,@(reverse current-list)) acc))))

      ;; Process next node
      (else (let* ((node (car rest-nodes))
                   (type (item-type node)))
              (if type
                  ;; Node is a list item
                  (if (or (not current-type)
                          (eq? type current-type))
                      ;; Same list type: keep accumulating
                      (loop (cdr rest-nodes)
                            (cons node current-list) type acc)
                      ;; List type changed: close current list and start new one
                      (loop (cdr rest-nodes)
                            (list node) type
                            (cons `(,current-type ,@(reverse current-list))
                                  acc)))
                  ;; Node is not a list item
                  (if (null? current-list)
                      ;; No active list
                      (loop (cdr rest-nodes)
                            '() #f
                            (cons node acc))
                      ;; Close active list and record current non-list node
                      (loop (cdr rest-nodes)
                            '() #f
                            (cons node
                                  (cons `(,current-type ,@(reverse
                                                           current-list)) acc))))))))))

(define (ast->html ast)
  (define (render node)
    (match node
      (('heading ('stars s)
        ('line-content text))
       (let ((lvl (string-length s)))
         (format #f "<h~a>~a</h~a>" lvl text lvl)))
      (('description-list items ...)
       (let ((dl-strings (map (lambda (li)
                                (match li
                                  (('unordered-item ('desc-key key)
                                      ('line-content text))
                                   (format #f "  <dt>~a</dt>\n  <dd>~a</dd>"
                                           key text))
                                  (_ ""))) items)))
         (format #f "<dl>\n~a\n</dl>"
                 (string-join dl-strings "\n"))))
      (('unordered-list items ...)
       (let ((li-strings (map (lambda (li)
                                (match li
                                  (('unordered-item "-"
                                    ('line-content text))
                                   (format #f "  <li>~a</li>" text))
                                  (('unordered-item ('line-content text))
                                   (format #f "  <li>~a</li>" text))
                                  (_ ""))) items)))
         (format #f "<ul>\n~a\n</ul>"
                 (string-join li-strings "\n"))))
      (('ordered-list items ...)
       (let ((li-strings (map (lambda (li)
                                (match li
                                  (('ordered-item ('line-content text))
                                   (format #f "  <li>~a</li>" text))
                                  (_ ""))) items)))
         (format #f "<ol>\n~a\n</ol>"
                 (string-join li-strings "\n"))))

      (('paragraph ('line-content text))
       (format #f "<p>~a</p>" text))
      (('src-block ('lang l)
        ('block-content bc))
       (format #f "<pre class=\"~a\">\n~a</pre>" l bc))
      (_ "")))

  (string-join (map render ast) "\n"))

;; =====================================================================
;; User-facing API
;; =====================================================================

(define (org->ast str)
  "Convert a Org-mode string into a AST."
  (let* ((match-result (match-pattern org-doc str))
         (ast (group-list-items (peg:tree match-result))))
    (pretty-print ast) ast))

(define (org-to-html str)
  "Convert a Org-mode string into HTML."
  (let* ((ast (org->ast str)))
    (ast->html ast)))
