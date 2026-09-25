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
  #:declarative? #t
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (org->ast inline-org->ast))

;; =====================================================================
;; Macros for PEG Pattern Generation
;; =====================================================================

(define-syntax-rule (define-block-pattern name begin-pat end-pat content-pat)
  (begin
    (define-peg-pattern content-pat all
                        (* (and (not-followed-by end-pat) peg-any)))
    (define-peg-pattern name all
                        (and begin-pat
                             eol
                             content-pat
                             end-pat
                             (* ws)
                             eol))))

(define-syntax-rule (define-inline-markup name name-delim name-body
                                          name-content delim)
  (begin
    (define-peg-pattern name-delim none delim)
    (define-peg-pattern name-body body
                        (or (and (not-followed-by (or delim " " "\t" nl))
                                 peg-any)
                            (and (or " " "\t")
                                 (not-followed-by (or delim nl)))))
    (define-peg-pattern name-content all
                        (+ name-body))
    (define-peg-pattern name body
                        (and name-delim
                             (not-followed-by (or " " "\t" nl)) name-content
                             name-delim))))

(define-syntax-rule (define-list-item name marker)
  (define-peg-pattern name all
                      (and item-indent marker
                           (+ ws) line-content eol)))

;; =====================================================================
;; Character & Whitespace Primitives
;; =====================================================================

(define-peg-pattern nl none
                    (or "\r\n" "\n"))
(define-peg-pattern ws none
                    (or " " "\t"))
(define-peg-pattern eof none
                    (not-followed-by peg-any))
(define-peg-pattern eol none
                    (or nl eof))
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
                        separator
                        table
                        src-block
                        quote-block
                        example-block
                        export-html-block
                        description-list
                        unordered-list
                        ordered-list
                        paragraph))

(define-peg-pattern empty-line none
                    (and (* ws) nl))

(define-peg-pattern separator all
                    (and (* ws) "-----"
                         (* "-")
                         (* ws) eol))

;; FIX: Wrap stars in a `body` pattern so they are retained in the AST
(define-peg-pattern heading-stars body
                    (+ "*"))
(define-peg-pattern heading all
                    (and heading-stars
                         (+ ws) line-content eol))

;; =====================================================================
;; Tables
;; =====================================================================

(define-peg-pattern table-sep none "|")
(define-peg-pattern table-column-sep none "+")
(define-peg-pattern table-cell all
                    (* (and (not-followed-by (or table-sep eol)) peg-any)))

(define-peg-pattern table-row-end none
                    (or (and table-sep
                             (* ws) eol)
                        (and (* ws) eol)))

(define-peg-pattern table-row-cells body
                    (and (followed-by table-sep)
                         (* (and table-sep
                                 (not-followed-by table-row-end) table-cell))
                         table-row-end))

(define-peg-pattern table-row all table-row-cells)

(define-peg-pattern table-separator-cell none
                    (and (* ws)
                         (+ "-")
                         (* ws)))

(define-peg-pattern table-separator-row all
                    (and table-sep table-separator-cell
                         (* (and table-column-sep table-separator-cell))
                         table-row-end))

(define-peg-pattern table-header-row all
                    (and table-row-cells
                         (followed-by table-separator-row)))

(define-peg-pattern table-line body
                    (or table-header-row table-separator-row table-row))

(define-peg-pattern table all
                    (+ table-line))

;; =====================================================================
;; List Items & Recursive List Containers
;; =====================================================================

(define-peg-pattern list-bullet none
                    (or "-" "+"))
(define-peg-pattern alpha none
                    (or (range #\a #\z)
                        (range #\A #\Z)))
(define-peg-pattern hierarchical-num none
                    (and (+ digit)
                         (* (and "."
                                 (+ digit)))))
(define-peg-pattern list-num none
                    (and (or hierarchical-num
                             (+ alpha))
                         (or "." ")")))
(define-peg-pattern desc-sep none
                    (and (+ ws) "::"
                         (or (+ ws)
                             (followed-by eol))))
(define-peg-pattern desc-key all
                    (+ (and (not-followed-by (or nl desc-sep eof)) peg-any)))
(define-peg-pattern item-indent body
                    (* (or " " "\t")))

(define-peg-pattern desc-item all
                    (and item-indent
                         (or list-bullet list-num)
                         (+ ws)
                         desc-key
                         desc-sep
                         line-content
                         eol))

(define-list-item unordered-item list-bullet)
(define-list-item ordered-item list-num)

(define-peg-pattern description-list all
                    (+ desc-item))
(define-peg-pattern unordered-list all
                    (+ unordered-item))
(define-peg-pattern ordered-list all
                    (+ ordered-item))

;; =====================================================================
;; Fixed-Format / Export Blocks
;; =====================================================================

(define-peg-pattern begin-quote none
                    (or "#+BEGIN_QUOTE" "#+begin_quote"))
(define-peg-pattern end-quote none
                    (or "#+END_QUOTE" "#+end_quote"))
(define-block-pattern quote-block begin-quote end-quote quote-content)

(define-peg-pattern begin-example none
                    (or "#+BEGIN_EXAMPLE" "#+begin_example"))
(define-peg-pattern end-example none
                    (or "#+END_EXAMPLE" "#+end_example"))
(define-block-pattern example-block begin-example end-example example-content)

(define-peg-pattern begin-export-html none
                    (or "#+BEGIN_EXPORT html" "#+begin_export html"
                        "#+BEGIN_EXPORT HTML" "#+begin_export HTML"))
(define-peg-pattern end-export none
                    (or "#+END_EXPORT" "#+end_export"))
(define-block-pattern export-html-block begin-export-html end-export
                      export-html-content)

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
                         src-content
                         end-src
                         (* ws)
                         eol))
(define-peg-pattern src-content all
                    (* (and (not-followed-by end-src) peg-any)))

;; ======================================================================
;; Paragraphs
;; ======================================================================

;; FIX: Require whitespace after block starters so that paragraphs
;; starting with `*bold*` aren't mistakenly discarded.
(define-peg-pattern special-starts none
                    (or (and (+ "*") ws)
                        (and (or "-" "+") ws)
                        (and (+ digit)
                             (or "." ")") ws)
                        "#+"
                        separator
                        empty-line))

(define-peg-pattern paragraph all
                    (and (not-followed-by special-starts) line-content eol))

(define-peg-pattern line-content all
                    (* (and (not-followed-by eol) peg-any)))

;; =====================================================================
;; Inline Org Markup
;; =====================================================================

(define-peg-pattern inline-escape body
                    (and "\\"
                         (or "*"
                             "/"
                             "_"
                             "~"
                             "="
                             "["
                             "]"
                             "\\")))

(define-inline-markup inline-code inline-code-delimiter inline-code-body
                      inline-code-content "~")
(define-inline-markup inline-verbatim inline-verbatim-delimiter
                      inline-verbatim-body inline-verbatim-content "=")
(define-inline-markup inline-italic inline-italic-delimiter inline-italic-body
                      inline-italic-content "/")
(define-inline-markup inline-bold inline-bold-delimiter inline-bold-body
                      inline-bold-content "*")
(define-inline-markup inline-underline inline-underline-delimiter
                      inline-underline-body inline-underline-content "_")

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

(define-peg-pattern inline-special body
                    (or inline-escape
                        inline-link-with-desc
                        inline-link-no-desc
                        inline-code
                        inline-verbatim
                        inline-italic
                        inline-bold
                        inline-underline))

(define-peg-pattern inline-text all
                    (+ (and (not-followed-by (or inline-special eol)) peg-any)))

(define-peg-pattern inline-element body
                    (or inline-special inline-text))
(define-peg-pattern inline-content body
                    (* inline-element))

;; =====================================================================
;; Helpers
;; =====================================================================

(define (process-ast ast)
  (match ast
    (((and list-type (or (quote unordered-list) (quote ordered-list) (quote description-list))) . items)
     (cons list-type
           (car (build-list-tree items 0))))
    ((node-type . children) (if (list? children)
                                (cons node-type
                                      (map process-ast children)) ast))
    (other other)))

(define (get-indent item)
  (match item
    ((_ (? string? spaces) . _) (string-length spaces))
    (_ 0)))

(define (strip-indent item)
  (match item
    ((tag (? string? spaces) . contents) (cons tag contents))
    (other other)))

(define (build-list-tree items depth)
  (let loop
    ((items items)
     (acc '()))
    (if (null? items)
        (cons (reverse acc)
              '())
        (let ((item-depth (get-indent (car items))))
          (cond
            ((< item-depth depth)
             (cons (reverse acc) items))
            ((> item-depth depth)
             (if (null? acc)
                 (loop (cdr items)
                       (cons (strip-indent (car items)) acc))
                 (match (build-list-tree items item-depth)
                   ((sub-items . rest) (let* ((prev-item (car acc))
                                              (updated-prev (append prev-item
                                                                    (list (cons 'nested-list
                                                                           sub-items)))))
                                         (loop rest
                                               (cons updated-prev
                                                     (cdr acc))))))))
            (else (loop (cdr items)
                        (cons (strip-indent (car items)) acc))))))))

;; =====================================================================
;; Parser Entry Point
;; =====================================================================

(define (inline-org->ast text)
  (peg:tree (match-pattern inline-content text)))

(define (org->ast str)
  (process-ast (peg:tree (match-pattern org-doc str))))
