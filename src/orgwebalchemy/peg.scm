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

(define-module (orgwebalchemy peg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
  #:export (org->ast inline-org->ast group-org-list-items))

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
                    (+ (and (not-followed-by (or nl desc-sep)) peg-any)))

(define-peg-pattern desc-body body
                    (and desc-key desc-sep line-content))

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
;; Inline Org Markup
;; =====================================================================

;; Escaped characters
;;
;; Org uses a backslash to escape markup characters:
;;
;;   \*not bold*
;;   \_not underline_
;;   \~not code~
;;
(define-peg-pattern inline-escape body
                    (and "\\" inline-escaped-character))

(define-peg-pattern inline-escaped-character all
                    (or "*"
                        "/"
                        "_"
                        "~"
                        "="
                        "["
                        "]"
                        "\\"))

;; Code: ~code~
(define-peg-pattern inline-code-delimiter none "~")

(define-peg-pattern inline-code-content all
                    (+ (and (not-followed-by "~")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-code body
                    (and inline-code-delimiter inline-code-content
                         inline-code-delimiter))

;; Verbatim: =verbatim=
(define-peg-pattern inline-verbatim-delimiter none "=")

(define-peg-pattern inline-verbatim-content all
                    (+ (and (not-followed-by "=")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-verbatim body
                    (and inline-verbatim-delimiter inline-verbatim-content
                         inline-verbatim-delimiter))

;; Italic: /italic/
(define-peg-pattern inline-italic-delimiter none "/")

(define-peg-pattern inline-italic-content all
                    (+ (and (not-followed-by "/")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-italic body
                    (and inline-italic-delimiter inline-italic-content
                         inline-italic-delimiter))

;; Bold: *bold*
(define-peg-pattern inline-bold-delimiter none "*")

(define-peg-pattern inline-bold-content all
                    (+ (and (not-followed-by "*")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-bold body
                    (and inline-bold-delimiter inline-bold-content
                         inline-bold-delimiter))

;; Underline: _underline_
(define-peg-pattern inline-underline-delimiter none "_")

(define-peg-pattern inline-underline-content all
                    (+ (and (not-followed-by "_")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-underline body
                    (and inline-underline-delimiter inline-underline-content
                         inline-underline-delimiter))

;; Org links:
;;
;;   [[https://example.com][Example]]
;;   [[https://example.com]]
;;
(define-peg-pattern link-open none "[[")
(define-peg-pattern link-close none "]]")
(define-peg-pattern link-sep none "][")

(define-peg-pattern link-target-content all
                    (+ (and (not-followed-by "]]")
                            (not-followed-by "][")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern link-description-content all
                    (+ (and (not-followed-by "]]")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-link-with-desc all
                    (and link-open link-target-content link-sep
                         link-description-content link-close))

(define-peg-pattern inline-link-no-desc all
                    (and link-open link-target-content link-close))

;; Plain inline text.
;;
;; Stop before the beginning of any inline construct.
(define-peg-pattern inline-text all
                    (+ (and (not-followed-by (or "\\"
                                                 "~"
                                                 "="
                                                 "/"
                                                 "*"
                                                 "_"
                                                 "[["
                                                 nl
                                                 eof)) peg-any)))

;; One inline element.
(define-peg-pattern inline-element body
                    (or inline-escape
                        inline-link-with-desc
                        inline-link-no-desc
                        inline-code
                        inline-verbatim
                        inline-italic
                        inline-bold
                        inline-underline
                        inline-text))

;; A complete inline sequence.
(define-peg-pattern inline-content body
                    (* inline-element))


;; =====================================================================
;; AST Transformation & Grouping
;; =====================================================================

(define (org-list-desc-item? node)
  "Return #t if NODE contains a 'desc-key child."
  (and (pair? node)
       (any (lambda (child)
              (and (pair? child)
                   (eq? (car child)
                        'desc-key)))
            (cdr node))))

(define (org-list-item-type node)
  "Determine the list grouping type for NODE."
  (cond
    ((not (pair? node))
     #f)

    ((org-list-desc-item? node)
     'description-list)

    ((eq? (car node)
          'unordered-item)
     'unordered-list)

    ((eq? (car node)
          'ordered-item)
     'ordered-list)

    (else #f)))

(define (group-org-list-items ast)
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
                   (type (org-list-item-type node)))
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


(define (inline-org->ast text)
  "Parse inline Org markup in TEXT."
  (let* ((ast (peg:tree (match-pattern inline-content text))))
    (newline)
    (pretty-print ast)
    (newline) ast))

(define (org->ast str)
  "Convert a Org-mode string into a AST."
  (let* ((match-result (match-pattern org-doc str))
         (ast (group-org-list-items (peg:tree match-result))))
    (newline)
    (pretty-print ast)
    (newline) ast))
