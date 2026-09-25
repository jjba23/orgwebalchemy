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

;; =====================================================================
;; Horizontal Rules / Separators
;; =====================================================================

;; Org-mode horizontal rule:
;;
;;   -----
;;   ----------
;;     --------
;;   -----
;;
;; A separator consists of at least five hyphens, optionally surrounded
;; by whitespace, followed by a newline or EOF.

(define-peg-pattern separator all
                    (and (* ws) "-----"
                         (* "-")
                         (* ws)
                         (or nl eof)))

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
;; Tables
;; =====================================================================

;; A table is delimited by vertical bar.
(define-peg-pattern table-sep none "|")

;; A table must start with a vertical bar.
(define-peg-pattern table-start none "|")

;; A table cell is everything up to the next `|`, newline, or EOF.
(define-peg-pattern table-cell all
                    (* (and (not-followed-by (or table-sep nl eof)) peg-any)))

(define-peg-pattern table-column-sep none "+")

;; Properly consumes the optional closing pipe, trailing spaces, and the newline.
(define-peg-pattern table-row-end none
                    (or (and table-sep
                             (* ws)
                             (or nl eof))
                        (and (* ws)
                             (or nl eof))))

;; A normal table row.
;; We use `(followed-by table-sep)` to ensure it starts with a pipe without
;; creating an extra nesting layer. The single `*` loop flattens the cells nicely.
(define-peg-pattern table-row all
                    (and (followed-by table-sep)
                         (* (and table-sep
                                 (not-followed-by table-row-end) table-cell))
                         table-row-end))

;; A table separator cell consists of dashes, optionally surrounded
;; by whitespace.
(define-peg-pattern table-separator-cell none
                    (and (* ws)
                         (+ "-")
                         (* ws)))

;; A separator row:
;; Ends properly using `table-row-end` so it doesn't leave unconsumed newlines.
(define-peg-pattern table-separator-row all
                    (and table-sep table-separator-cell
                         (* (and table-column-sep table-separator-cell))
                         table-row-end))

;; A header row is a normal row immediately followed by a table separator row.
(define-peg-pattern table-header-row all
                    (and (followed-by table-sep)
                         (* (and table-sep
                                 (not-followed-by table-row-end) table-cell))
                         table-row-end
                         (followed-by table-separator-row)))

;; Update table-line to recognize header rows first.
(define-peg-pattern table-line body
                    (or table-header-row table-separator-row table-row))

;; A complete table consists of one or more consecutive table lines.
(define-peg-pattern table all
                    (+ table-line))

;; =====================================================================
;; List Items & Recursive List Containers
;; =====================================================================

(define-peg-pattern list-bullet none
                    (or "-" "+"))

;; Recognize upper and lower case letters
(define-peg-pattern alpha none
                    (or (range #\a #\z)
                        (range #\A #\Z)))

;; Recognize digits separated by dots (e.g., "1", "1.1", "1.2.3")
;; It greedily grabs "digit(s)" optionally followed by ".digit(s)"
(define-peg-pattern hierarchical-num none
                    (and (+ digit)
                         (* (and "."
                                 (+ digit)))))

;; An ordered list bullet is either a hierarchical number or alphabetical characters,
;; followed by a period or a closing parenthesis.
(define-peg-pattern list-num none
                    (and (or hierarchical-num
                             (+ alpha))
                         (or "." ")")))

(define-peg-pattern desc-sep none
                    (and (+ ws) "::"
                         (or (+ ws)
                             (followed-by nl)
                             (followed-by eof))))

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
                         (or nl eof)))

(define-peg-pattern unordered-item all
                    (and item-indent list-bullet
                         (+ ws) line-content
                         (or nl eof)))

(define-peg-pattern ordered-item all
                    (and item-indent list-num
                         (+ ws) line-content
                         (or nl eof)))

;; The top-level lists are just flat sequences of items
(define-peg-pattern description-list all
                    (+ desc-item))
(define-peg-pattern unordered-list all
                    (+ unordered-item))
(define-peg-pattern ordered-list all
                    (+ ordered-item))

;; =====================================================================
;; Fixed-Format / Export Blocks
;; =====================================================================

;; #+BEGIN_QUOTE ... #+END_QUOTE
(define-peg-pattern begin-quote none
                    (or "#+BEGIN_QUOTE" "#+begin_quote"))

(define-peg-pattern end-quote none
                    (or "#+END_QUOTE" "#+end_quote"))

;; #+BEGIN_EXAMPLE ... #+END_EXAMPLE
(define-peg-pattern begin-example none
                    (or "#+BEGIN_EXAMPLE" "#+begin_example"))

(define-peg-pattern end-example none
                    (or "#+END_EXAMPLE" "#+end_example"))

;; #+BEGIN_EXPORT html ... #+END_EXPORT
(define-peg-pattern begin-export-html none
                    (or "#+BEGIN_EXPORT html" "#+begin_export html"
                        "#+BEGIN_EXPORT HTML" "#+begin_export HTML"))

(define-peg-pattern end-export none
                    (or "#+END_EXPORT" "#+end_export"))

(define-peg-pattern quote-content all
                    (* (and (not-followed-by end-quote) peg-any)))

(define-peg-pattern example-content all
                    (* (and (not-followed-by end-example) peg-any)))

(define-peg-pattern export-html-content all
                    (* (and (not-followed-by end-export) peg-any)))

(define-peg-pattern quote-block all
                    (and begin-quote
                         (or nl eof)
                         quote-content
                         end-quote
                         (* ws)
                         (or nl eof)))

(define-peg-pattern example-block all
                    (and begin-example
                         (or nl eof)
                         example-content
                         end-example
                         (* ws)
                         (or nl eof)))

(define-peg-pattern export-html-block all
                    (and begin-export-html
                         (or nl eof)
                         export-html-content
                         end-export
                         (* ws)
                         (or nl eof)))

;; =====================================================================
;; Source Blocks (#+BEGIN_SRC ... #+END_SRC)
;; =====================================================================

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
                         (or nl eof)))

(define-peg-pattern src-content all
                    (* (and (not-followed-by end-src) peg-any)))

;; ======================================================================
;; Paragraphs (Fallback: lines not starting with special markup)
;; ======================================================================
(define-peg-pattern special-starts none
                    (or "*"
                        "-"
                        "+"
                        "#+"
                        (and (+ digit)
                             (or "." ")"))
                        separator
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
(define-peg-pattern inline-code-body body
                    (or (and (not-followed-by (or "~" " " "\t" nl)) peg-any)
                        (and (or " " "\t")
                             (not-followed-by (or "~" nl)))))
(define-peg-pattern inline-code-content all
                    (+ inline-code-body))
(define-peg-pattern inline-code body
                    (and inline-code-delimiter
                         (not-followed-by (or " " "\t" nl))
                         inline-code-content inline-code-delimiter))

;; Verbatim: =verbatim=
(define-peg-pattern inline-verbatim-delimiter none "=")
(define-peg-pattern inline-verbatim-body body
                    (or (and (not-followed-by (or "=" " " "\t" nl)) peg-any)
                        (and (or " " "\t")
                             (not-followed-by (or "=" nl)))))
(define-peg-pattern inline-verbatim-content all
                    (+ inline-verbatim-body))
(define-peg-pattern inline-verbatim body
                    (and inline-verbatim-delimiter
                         (not-followed-by (or " " "\t" nl))
                         inline-verbatim-content inline-verbatim-delimiter))

;; Italic: /italic/
(define-peg-pattern inline-italic-delimiter none "/")
(define-peg-pattern inline-italic-body body
                    (or (and (not-followed-by (or "/" " " "\t" nl)) peg-any)
                        (and (or " " "\t")
                             (not-followed-by (or "/" nl)))))
(define-peg-pattern inline-italic-content all
                    (+ inline-italic-body))
(define-peg-pattern inline-italic body
                    (and inline-italic-delimiter
                         (not-followed-by (or " " "\t" nl))
                         inline-italic-content inline-italic-delimiter))

;; Bold: *bold*
(define-peg-pattern inline-bold-delimiter none "*")
(define-peg-pattern inline-bold-body body
                    (or (and (not-followed-by (or "*" " " "\t" nl)) peg-any)
                        (and (or " " "\t")
                             (not-followed-by (or "*" nl)))))
(define-peg-pattern inline-bold-content all
                    (+ inline-bold-body))
(define-peg-pattern inline-bold body
                    (and inline-bold-delimiter
                         (not-followed-by (or " " "\t" nl))
                         inline-bold-content inline-bold-delimiter))

;; Underline: _underline_
(define-peg-pattern inline-underline-delimiter none "_")
(define-peg-pattern inline-underline-body body
                    (or (and (not-followed-by (or "_" " " "\t" nl)) peg-any)
                        (and (or " " "\t")
                             (not-followed-by (or "_" nl)))))
(define-peg-pattern inline-underline-content all
                    (+ inline-underline-body))
(define-peg-pattern inline-underline body
                    (and inline-underline-delimiter
                         (not-followed-by (or " " "\t" nl))
                         inline-underline-content inline-underline-delimiter))

;; Org links:
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
(define-peg-pattern inline-text all
                    (+ (and (not-followed-by (or inline-escape
                                                 inline-link-with-desc
                                                 inline-link-no-desc
                                                 inline-code
                                                 inline-verbatim
                                                 inline-italic
                                                 inline-bold
                                                 inline-underline
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
;; Helpers
;; =====================================================================

;; Recursively walk the AST and process any flat lists
(define (process-ast ast)
  (match ast
    (((quote unordered-list) . items) (cons 'unordered-list
                                            (car (build-list-tree items 0))))
    (((quote ordered-list) . items) (cons 'ordered-list
                                          (car (build-list-tree items 0))))
    (((quote description-list) . items) (cons 'description-list
                                              (car (build-list-tree items 0))))
    ((node-type . children) (if (list? children)
                                (cons node-type
                                      (map process-ast children)) ast))
    (other other)))

;; If the first element inside the item is a string, measure its length.
;; Otherwise (no indentation string present), return 0.
(define (get-indent item)
  (match item
    ((_ (? string? spaces) . _) (string-length spaces))
    (_ 0)))

;; Strip the leading indentation string from the AST node
(define (strip-indent item)
  (match item
    ((tag (? string? spaces) . contents) (cons tag contents))
    (other other)))

;; Recursive function to fold flat items into a tree
(define (build-list-tree items depth)
  (let loop
    ((items items)
     (acc '()))
    (if (null? items)
        ;; Return nested structure and remaining unparsed items
        (cons (reverse acc)
              '())
        (let ((item-depth (get-indent (car items))))
          (cond
            ;; If depth decreases, we are done with this nested level
            ((< item-depth depth)
             (cons (reverse acc) items))

            ;; If depth increases, we recurse and append to the PREVIOUS item
            ((> item-depth depth)
             (if (null? acc)
                 ;; Fallback for strangely formatted first-items
                 (loop (cdr items)
                       (cons (strip-indent (car items)) acc))
                 (match (build-list-tree items item-depth)
                   ((sub-items . rest) (let* ((prev-item (car acc))
                                              ;; Attach the newly nested list into the previous item
                                              (updated-prev (append prev-item
                                                                    (list (cons 'nested-list
                                                                           sub-items)))))
                                         (loop rest
                                               (cons updated-prev
                                                     (cdr acc))))))))

            ;; Same depth, just add to accumulator and continue
            (else (loop (cdr items)
                        (cons (strip-indent (car items)) acc))))))))

;; =====================================================================
;; Parser Entry Point
;; =====================================================================

(define (inline-org->ast text)
  "Parse inline Org markup in TEXT."
  (let ((ast (peg:tree (match-pattern inline-content text))))
    ;; (newline)
    ;; (pretty-print ast)
    ;; (newline)
    ast))

(define (org->ast str)
  "Convert an Org-mode string directly into an AST with nested list structures."
  (let* ((ast (peg:tree (match-pattern org-doc str)))
         (processed-ast (process-ast ast)))
    ;; (newline)
    ;; (pretty-print processed-ast)
    ;; (newline)
    processed-ast))
