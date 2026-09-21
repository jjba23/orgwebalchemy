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

;; =====================================================================
;; Core Document Structure
;; =====================================================================
(define-peg-pattern org-doc body
                    (* element))
(define-peg-pattern element body
                    (or empty-line heading src-block list-item paragraph))
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

;; Unordered List Items (- item)
(define-peg-pattern list-item all
                    (and "-"
                         (+ ws) line-content
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

(define (group-list-items ast)
  (let loop
    ((rest-nodes ast)
     (current-list '())
     (acc '()))
    (cond
      ;; 1. We reached the end of the document
      ((null? rest-nodes)
       (reverse (if (null? current-list) acc
                    (cons `(unordered-list ,@(reverse current-list)) acc))))

      ;; 2. Process the next node
      (else (let ((node (car rest-nodes)))
              (if (and (pair? node)
                       (eq? (car node)
                            'list-item))
                  ;; A. It's a list item: save it in our current-list bucket and continue
                  (loop (cdr rest-nodes)
                        (cons node current-list) acc)

                  ;; B. It's NOT a list item
                  (if (null? current-list)
                      ;; B1. We aren't currently tracking a list; just push the node to acc
                      (loop (cdr rest-nodes)
                            '()
                            (cons node acc))
                      ;; B2. We WERE tracking a list. Close the list, then push the new node
                      (loop (cdr rest-nodes)
                            '()
                            (cons node
                                  (cons `(unordered-list ,@(reverse
                                                            current-list)) acc))))))))))

(define (ast->html ast)
  (define (render node)
    (match node
      (('heading ('stars s)
        ('line-content text))
       (let ((lvl (string-length s)))
         (format #f "<h~a>~a</h~a>" lvl text lvl)))
      (('unordered-list items ...)
       (let ((li-strings (map (lambda (li)
                                (match li
                                  (('list-item "-"
                                    ('line-content text))
                                   (format #f "  <li>~a</li>" text))
                                  (('list-item ('line-content text))
                                   (format #f "  <li>~a</li>" text))
                                  (_ ""))) items)))
         (format #f "<ul>\n~a\n</ul>"
                 (string-join li-strings "\n"))))
      (('paragraph ('line-content text))
       (format #f "<p>~a</p>" text))
      (('src-block ('lang l) ('block-content bc))
       (format #f "<pre class=\"~a\">\n~a</pre>" l bc))
      (_ "")))

  (string-join (map render ast) "\n"))

;; =====================================================================
;; User-facing API
;; =====================================================================

(define (org-to-html str)
  "Convert a Org-mode string into HTML."
  (let* ((match-result (match-pattern org-doc str))
         (ast (group-list-items (peg:tree match-result))))
    (display "--- Parsing Org Text ---\n")
    (pretty-print ast)
    (ast->html ast)))
