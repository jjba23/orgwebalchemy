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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (orgwebalchemy peg)
  #:export (org->markdown org-ast->markdown inline-org->markdown))

;; =======================================================================
;; Helpers
;; =======================================================================

(define (trim-whitespace str)
  "Return @var{str} without leading or trailing spaces and tabs."
  (let ((n (string-length str)))
    (let skip-leading
      ((i 0))
      (if (and (< i n)
               (or (char=? (string-ref str i) #\space)
                   (char=? (string-ref str i) #\tab)))
          (skip-leading (+ i 1))
          (let skip-trailing
            ((j n))
            (if (and (> j i)
                     (or (char=? (string-ref str
                                             (- j 1)) #\space)
                         (char=? (string-ref str
                                             (- j 1)) #\tab)))
                (skip-trailing (- j 1))
                (substring str i j)))))))

(define (prefix-lines prefix text)
  "Return @var{text} with @var{prefix} added in front of every non-empty line."
  (let ((lines (string-split (string-trim-right text
                                                (lambda (c)
                                                  (char=? c #\newline)))
                             #\newline)))
    (string-join (map (lambda (line)
                        (if (string-null? line) prefix
                            (string-append prefix line))) lines) "\n")))

(define (indent-lines indent text)
  "Return @var{text} with @var{indent} added in front of every line."
  (let ((lines (string-split (string-trim-right text
                                                (lambda (c)
                                                  (char=? c #\newline)))
                             #\newline)))
    (string-join (map (lambda (line)
                        (string-append indent line)) lines) "\n")))

(define (split-checkbox text)
  "Return @code{(state . rest)} when @var{text} starts with an Org checkbox.
@var{state} is @code{#t} for a checked box and @code{#f} otherwise."
  (match (string-match "^\\[([ xX])\\][[:space:]]+" text)
    (#f #f)
    (m (cons (let ((c (match:substring m 1)))
               (or (string=? c "x")
                   (string=? c "X")))
             (substring text
                        (match:end m 0))))))

;; =======================================================================
;; Inline Org AST -> Markdown
;; =======================================================================

(define (inline-nodes ast)
  "Normalise @var{ast} into a list of inline nodes."
  (cond
    ((string? ast)
     (list ast))
    ((and (pair? ast)
          (symbol? (car ast)))
     (list ast))
    (else ast)))

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

    ;; [[URL][description]] and [[URL]]
    (('inline-link-with-desc ('link-target-content url)
      ('link-description-content description))
     (format #f "[~a](~a)"
             (inline-render-token description) url))
    (('inline-link-no-desc ('link-target-content url))
     (format #f "[~a](~a)" url url))

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

(define (heading-parts children)
  "Extract @code{(todo priority tags text)} from a heading's @var{children}."
  (let ((todo (any (lambda (c)
                     (match c
                       (('heading-todo x)
                        x)
                       (_ #f))) children))
        (priority (any (lambda (c)
                         (match c
                           (('heading-priority x)
                            x)
                           (_ #f))) children))
        (tags (append-map (lambda (c)
                            (match c
                              (('heading-tags xs ...)
                               xs)
                              (_ '()))) children))
        (text (any (lambda (c)
                     (match c
                       (('line-content x)
                        x)
                       (_ #f))) children)))
    (list todo priority tags text)))

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
  "Render Org table @var{rows} as a GitHub-flavoured Markdown table."
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
         (body-rows (if header body
                        (cdr body))))
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

(define (checkbox-marker checkbox)
  "Return the Markdown list marker prefix for @var{checkbox}."
  (cond
    ((not checkbox)
     "- ")
    ((car checkbox)
     "- [x] ")
    (else "- [ ] ")))

(define (list-item-kind item)
  "Return the kind of a list @var{item}: @code{unordered}, @code{ordered},
@code{description} or @code{other}."
  (match item
    (((quote unordered-item) . _) 'unordered)
    (((quote ordered-item) . _) 'ordered)
    (((quote desc-item) . _) 'description)
    (_ 'other)))

(define (group-list-items items)
  "Group consecutive @var{items} of the same kind into @code{(kind . items)}
runs, preserving their original order."
  (let loop
    ((items items)
     (kind #f)
     (acc '())
     (groups '()))
    (cond
      ((null? items)
       (reverse (if (null? acc) groups
                    (cons (cons kind
                                (reverse acc)) groups))))
      ((eq? (list-item-kind (car items)) kind)
       (loop (cdr items) kind
             (cons (car items) acc) groups))
      (else (loop (cdr items)
                  (list-item-kind (car items))
                  (list (car items))
                  (if (null? acc) groups
                      (cons (cons kind
                                  (reverse acc)) groups)))))))

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
                                          (checkbox-marker checkbox)
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
                                          (cdr checkbox) text))
                                (marker (cond
                                          ((not checkbox)
                                           "")
                                          ((car checkbox)
                                           "[x] ")
                                          (else "[ ] "))))
                           (string-append indent
                                          (format #f "~a. "
                                                  (+ i 1))
                                          marker
                                          (inline-org->markdown (inline-org->ast
                                                                 body))
                                          "\n"
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
     (string-append (indent-lines "    " text) "\n\n"))

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
  "Convert a document Org AST @var{ast} into a Markdown string."
  (string-join (filter-map render-block
                           (inline-nodes ast)) ""))

(define (org->markdown str)
  "Convert an Org-mode string @var{str} into a Markdown string."
  (org-ast->markdown (org->ast str)))
