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

(define-module (orgwebalchemy markdown)
  #:declarative? #t
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (orgwebalchemy peg)
  #:export (org->markdown org-ast->markdown inline-org->markdown))

;; =======================================================================
;; Helpers
;; =======================================================================

(define (prefix-lines prefix text)
  "Return @var{text} with @var{prefix} added in front of every line."
  (let ((lines (string-split (string-trim-right text
                                                (lambda (c)
                                                  (char=? c #\newline)))
                             #\newline)))
    (string-join (map (lambda (line)
                        (string-append prefix line)) lines) "\n")))

;; =======================================================================
;; Inline Org AST -> Markdown
;; =======================================================================

(define (inline-render-token token)
  "Render a token that may itself contain nested inline Org markup."
  (inline-org->markdown (inline-org->ast token)))

(define (render-inline node)
  "Render one inline Org @var{node} to a Markdown string."
  (match node
    ;; Escaped markup
    (('inline-escape-char text)
     text)
    (('inline-escape text)
     text)

    ;; ~code~ and =verbatim=
    (('inline-code-content text)
     (format #f "`~a`" text))
    (('inline-verbatim-content text)
     (format #f "`~a`" text))

    ;; /italic/
    (('inline-italic-content text)
     (format #f "*~a*"
             (inline-render-token text)))

    ;; *bold*
    (('inline-bold-content text)
     (format #f "**~a**"
             (inline-render-token text)))

    ;; _underline_
    (('inline-underline-content text)
     (format #f "<u>~a</u>"
             (inline-render-token text)))

    ;; +strikethrough+
    (('inline-strike-content text)
     (string-append "~~"
                    (inline-render-token text) "~~"))

    ;; x^{superscript} / x_{subscript}
    (('inline-superscript-content text)
     (format #f "<sup>~a</sup>"
             (inline-render-token text)))
    (('inline-subscript-content text)
     (format #f "<sub>~a</sub>"
             (inline-render-token text)))

    ;; A forced line break is two trailing spaces.
    (('inline-line-break _)
     "  \n")

    ;; [[target][description]] and [[target]].  An image link becomes
    ;; Markdown's ![alt](src); every other link uses the href the shared link
    ;; classifier produced, so `file:' links lose their prefix and `*Heading'
    ;; links become the fragment Markdown renderers slug the heading to.
    (('inline-link-with-desc ('link-target-content url)
      ('link-description-content description))
     (if (org-image-link? url)
         (format #f "![~a](~a)"
                 (inline-render-token description)
                 (org-link-href url))
         (format #f "[~a](~a)"
                 (inline-render-token description)
                 (org-link-href url))))
    (('inline-link-no-desc ('link-target-content url))
     (if (org-image-link? url)
         (format #f "![~a](~a)"
                 (org-link-display url)
                 (org-link-href url))
         (format #f "[~a](~a)"
                 (org-link-display url)
                 (org-link-href url))))

    ;; Bare URL in prose, as a Markdown autolink.
    (('inline-raw-url ('url-scheme scheme) rest)
     (format #f "<~a~a>" scheme rest))

    ;; Footnotes
    (('inline-footnote-ref ('footnote-label label))
     (format #f "[^~a]" label))
    (('inline-footnote-def ('footnote-label label)
      ('footnote-content definition))
     (format #f "[^~a]: ~a" label
             (inline-render-token definition)))
    (('inline-footnote-inline ('footnote-content definition))
     (format #f "^[~a]"
             (inline-render-token definition)))

    ;; Plain text and raw strings
    (('inline-text text)
     text)
    ((? string? text)
     text)
    (_ #f)))

(define (inline-org->markdown ast)
  "Convert an inline Org AST @var{ast} into a Markdown string."
  (string-join (filter-map render-inline
                           (inline-nodes ast)) ""))

;; =======================================================================
;; Block Org AST -> Markdown
;; =======================================================================

(define (render-heading stars children)
  "Render a heading with @var{stars} and @var{children} to Markdown."
  (let* ((parts (heading-parts children))
         (todo (list-ref parts 0))
         (priority (list-ref parts 1))
         (tags (list-ref parts 2))
         (text (or (list-ref parts 3) "")))
    (string-append (make-string (string-length stars) #\#) " "
                   (string-join (filter-map identity
                                            (list todo
                                                  (and priority
                                                       (format #f "[#~a]"
                                                               priority))
                                                  (inline-org->markdown (inline-org->ast
                                                                         text))
                                                  (and (pair? tags)
                                                       (string-append ":"
                                                                      (string-join
                                                                       tags
                                                                       ":")
                                                                      ":"))))
                                " ") "\n\n")))

(define (cell-text cell)
  "Return the trimmed text of a table @var{cell}."
  (match cell
    (('table-cell text)
     (trim-whitespace text))
    (_ "")))

(define (row-cells row)
  "Return the cells of a table @var{row} as strings."
  (match row
    ((_ cells ...)
     (map cell-text cells))
    (_ '())))

(define (render-table rows)
  "Render Org table @var{rows} as a GitHub-flavoured Markdown table.
The columns are the ones the header row has -- the row Org marks as a header,
or the first row when it marks none -- and every row is then written as it is,
however many cells it holds.  A table without a single cell renders as
nothing."
  (let* ((header (find (lambda (r)
                         (match r
                           (((quote table-header-row) . _) #t)
                           (_ #f))) rows))
         (body (filter (lambda (r)
                         (match r
                           (((quote table-row) . _) #t)
                           (_ #f))) rows))
         (header-cells (cond
                         (header (row-cells header))
                         ((null? body)
                          '())
                         (else (row-cells (car body)))))
         (body-rows (cond
                      (header body)
                      ((null? body)
                       '())
                      (else (cdr body)))))
    (if (null? header-cells) ""
        (string-append "| "
                       (string-join header-cells " | ")
                       " |\n"
                       "| "
                       (string-join (map (lambda (_)
                                           "---") header-cells) " | ")
                       " |\n"
                       (string-join (map (lambda (row)
                                           (string-append "| "
                                                          (string-join (row-cells
                                                                        row)
                                                                       " | ")
                                                          " |\n")) body-rows)
                                    "")
                       "\n"))))

(define (checkbox-marker checkbox prefix)
  "Return the markdown list @var{prefix} with the marker for @var{checkbox}.
@var{checkbox} is @code{#f} for an item that is not a checkbox at all."
  (cond
    ((not checkbox)
     prefix)
    ((car checkbox)
     (string-append prefix "[x] "))
    (else (string-append prefix "[ ] "))))

(define (render-list-markdown items indent)
  "Render a list's @var{items} at @var{indent}, starting a new Markdown list
whenever the item kind changes."
  (string-join (map (lambda (group)
                      (let ((kind (car group))
                            (run (cdr group)))
                        (cond
                          ((eq? kind
                                'unordered)
                           (render-unordered run indent))
                          ((eq? kind
                                'ordered)
                           (render-ordered run indent))
                          ((eq? kind
                                'description)
                           (render-description run indent))
                          (else ""))))
                    (group-list-items items)) ""))

(define (render-nested sub indent)
  "Render the nested lists found in a list item's @var{sub} nodes."
  (string-join (map (lambda (node)
                      (match node
                        (('nested-list items ...)
                         (render-list-markdown items indent))
                        (_ (org-ast->markdown node)))) sub) ""))

(define (render-unordered items indent)
  "Render unordered @var{items} at @var{indent}."
  (string-join (map (lambda (item)
                      (match item
                        (('unordered-item ('line-content text) sub ...)
                         (let* ((checkbox (split-checkbox text))
                                (body (if checkbox
                                          (cdr checkbox) text)))
                           (string-append indent
                                          (checkbox-marker checkbox "- ")
                                          (inline-org->markdown (inline-org->ast
                                                                 body)) "\n"
                                          (render-nested sub
                                                         (string-append indent
                                                          "  ")))))
                        (_ ""))) items) ""))

(define (render-ordered items indent)
  "Render ordered @var{items} at @var{indent}."
  (string-join (map (lambda (item i)
                      (match item
                        (('ordered-item ('line-content text) sub ...)
                         (let* ((checkbox (split-checkbox text))
                                (body (if checkbox
                                          (cdr checkbox) text)))
                           (string-append indent
                                          (checkbox-marker checkbox
                                                           (format #f "~a. "
                                                                   (+ i 1)))
                                          (inline-org->markdown (inline-org->ast
                                                                 body)) "\n"
                                          (render-nested sub
                                                         (string-append indent
                                                          "   ")))))
                        (_ ""))) items
                    (iota (length items))) ""))

(define (render-description items indent)
  "Render description @var{items} at @var{indent}."
  (string-join (map (lambda (item)
                      (match item
                        (('desc-item ('desc-key key)
                          ('line-content text) sub ...)
                         (string-append indent
                                        "**"
                                        (inline-org->markdown (inline-org->ast
                                                               key))
                                        "**\n"
                                        indent
                                        ":   "
                                        (inline-org->markdown (inline-org->ast
                                                               text))
                                        "\n"
                                        (render-nested sub
                                                       (string-append indent
                                                                      "    "))))
                        (_ ""))) items) ""))

(define (render-block node)
  "Render one block Org @var{node} to a Markdown string."
  (match node
    ;; Headings
    (((quote heading) (? string? stars) . children) (render-heading stars
                                                                    children))

    ;; Horizontal separator
    (('separator _)
     "---\n\n")

    ;; Paragraph
    (('paragraph ('line-content text))
     (string-append (inline-org->markdown (inline-org->ast text)) "\n\n"))

    ;; Figure: the image with the caption and name the keywords above it gave
    ;; it.  Markdown has no figure element, so the name becomes an explicit
    ;; anchor -- the one thing a `[[fig:arch]]' link needs to resolve -- and the
    ;; caption an emphasised line under the image.
    (('figure _ ...)
     (match (figure-parts node)
       ((name caption target description)
        (string-append (if name
                           (format #f "<a id=\"~a\"></a>\n\n" name) "")
                       (format #f "![~a](~a)\n\n"
                               (or description caption
                                   (org-link-display target))
                               (org-link-href target))
                       (if caption
                           (format #f "*~a*\n\n" caption) "")))))

    ;; A named element: Markdown has no id on most of these elements, so the
    ;; name becomes an explicit anchor -- the thing a `[[tbl:data]]' or
    ;; `[[name]]' link needs to resolve -- and the element follows it.
    (('named _ ...)
     (match (named-parts node)
       ((name inner)
        (string-append (format #f "<a id=\"~a\"></a>\n\n" name)
                       (or (render-block inner) "")))))

    ;; Source block
    (('src-block ('lang l)
      ('src-content content))
     (string-append "```"
                    (if l l "") "\n" content "```\n\n"))

    ;; Example block (literal)
    (('example-block ('example-content content))
     (string-append "```\n" content "```\n\n"))

    ;; Quote block
    (('quote-block ('quote-content content))
     (string-append (prefix-lines "> " content) "\n\n"))

    ;; Verse block
    (('verse-block ('verse-content content))
     (let ((lines (string-split (string-trim-right content
                                                   (lambda (c)
                                                     (char=? c #\newline)))
                                #\newline)))
       (string-append (string-join lines "  \n") "\n\n")))

    ;; Center block
    (('center-block ('center-content content))
     (string-append "<div align=\"center\">\n\n"
                    (inline-org->markdown (inline-org->ast (string-trim-right
                                                            content
                                                            (lambda (c)
                                                              (char=? c
                                                                      #\newline)))))
                    "\n\n</div>\n\n"))

    ;; Raw HTML export
    (('export-html-block ('export-html-content html))
     (string-append (string-trim-right html
                                       (lambda (c)
                                         (char=? c #\newline))) "\n\n"))

    ;; Comments
    (('comment-line ('comment-content text))
     (string-append "<!-- " text " -->\n\n"))
    (('comment-block ('comment-block-content text))
     (string-append "<!-- " text "-->\n\n"))

    ;; Keyword / metadata
    (('keyword ('keyword-key key)
      ('keyword-value value))
     (string-append key ": " value "\n\n"))

    ;; Fixed width
    (('fixed-width ('fixed-width-content text))
     (string-append (prefix-lines "    " text) "\n\n"))

    ;; Lists, which may mix item kinds and nest lists of a different kind.
    (('org-list items ...)
     (string-append (render-list-markdown items "") "\n"))
    (('nested-list items ...)
     (string-append (render-list-markdown items "") "\n"))

    ;; Tables
    (('table rows ...)
     (render-table rows))

    (_ #f)))

(define (org-ast->markdown ast)
  "Convert a document Org AST @var{ast} into a Markdown string.
The document's own `#+NAME:' targets are in scope while it renders, so a bare
`[[name]]' link is parsed as the cross-reference it is."
  (parameterize ((element-names (document-names ast)))
    (string-join (filter-map render-block
                             (inline-nodes ast)) "")))

(define (org->markdown str)
  "Convert an Org-mode string @var{str} into a Markdown string."
  (org-ast->markdown (org->ast str)))
