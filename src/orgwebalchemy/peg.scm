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
  #:export (org->ast inline-org->ast
                     org-link-parts
                     org-link-protocol
                     org-link-path
                     org-link-search
                     org-link-href
                     org-link-display
                     org-image-link?
                     org-anchor))

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
;; A single-character newline test.  Scanning text with `(not-followed-by nl)'
;; costs a two-character and a one-character comparison at every position, and
;; then an `eof' check; `nl-char' halves that and is what `line-content' and
;; `inline-text' use to know where to stop.
(define-peg-pattern nl-char none
                    (or "\r" "\n"))
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
                        verse-block
                        center-block
                        example-block
                        export-html-block
                        comment-block
                        keyword
                        comment-line
                        fixed-width
                        org-list
                        paragraph))

(define-peg-pattern empty-line none
                    (and (* ws) nl))

(define-peg-pattern separator all
                    (and (* ws) "-----"
                         (* "-")
                         (* ws) eol))

;; Wrap stars in a `body` pattern so they are retained in the AST
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
;; `table-sep' is `|' (124), so the fast range stops short of it and resumes
;; after it; the checked branch handles that one character, the line ends and
;; anything outside printable ASCII.
(define-peg-pattern table-cell all
                    (* (or (range #\space #\{)
                           (range #\} #\~) "\t"
                           (and (not-followed-by (or table-sep eol)) peg-any))))

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

;; A single `list' element may hold items of any kind, in any mix.  Org lets a
;; list change its bullet type part-way through and lets a nested list use a
;; different type from its parent, so one production covers all of them and the
;; renderers group consecutive items into the right HTML/Markdown list.
(define-peg-pattern org-list all
                    (+ (or desc-item unordered-item ordered-item)))

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

;; =====================================================================
;; Verse, Center & Comment Blocks
;; =====================================================================

(define-peg-pattern begin-verse none
                    (or "#+BEGIN_VERSE" "#+begin_verse"))
(define-peg-pattern end-verse none
                    (or "#+END_VERSE" "#+end_verse"))
(define-block-pattern verse-block begin-verse end-verse verse-content)

(define-peg-pattern begin-center none
                    (or "#+BEGIN_CENTER" "#+begin_center"))
(define-peg-pattern end-center none
                    (or "#+END_CENTER" "#+end_center"))
(define-block-pattern center-block begin-center end-center center-content)

(define-peg-pattern begin-comment none
                    (or "#+BEGIN_COMMENT" "#+begin_comment"))
(define-peg-pattern end-comment none
                    (or "#+END_COMMENT" "#+end_comment"))
(define-block-pattern comment-block begin-comment end-comment
                      comment-block-content)

;; =====================================================================
;; Keywords, Comments & Fixed-Width Lines
;; =====================================================================

;; Whitespace patterns that leave no trace in the AST.
(define-peg-pattern lead-ws none
                    (* ws))
(define-peg-pattern sep-ws none
                    (+ ws))

;; In-buffer settings and metadata such as `#+TITLE:` or `#+AUTHOR:`.
;; The key is restricted so that block delimiters (`#+BEGIN_SRC`) can never be
;; mistaken for a keyword.
(define-peg-pattern keyword-key all
                    (+ (and (not-followed-by (or ws nl ":")) peg-any)))
(define-peg-pattern keyword-value all
                    (* (and (not-followed-by eol) peg-any)))
(define-peg-pattern keyword-lead none "#+")
(define-peg-pattern keyword-colon none ":")
(define-peg-pattern keyword all
                    (and lead-ws
                         keyword-lead
                         keyword-key
                         keyword-colon
                         lead-ws
                         keyword-value
                         eol))

;; Lines beginning with `#' (but not `#+') are comments and are not exported.
(define-peg-pattern comment-content all
                    (* (and (not-followed-by eol) peg-any)))
(define-peg-pattern comment-hash none "#")
(define-peg-pattern comment-line all
                    (and lead-ws
                         comment-hash
                         (not-followed-by "+")
                         (or sep-ws
                             (followed-by eol))
                         comment-content
                         eol))

;; Lines beginning with `: ' are fixed-width textual lines.
(define-peg-pattern fixed-width-content all
                    (* (and (not-followed-by eol) peg-any)))
(define-peg-pattern fixed-width-colon none ":")
(define-peg-pattern fixed-width all
                    (and lead-ws fixed-width-colon
                         (or sep-ws
                             (followed-by eol)) fixed-width-content eol))

;; ======================================================================
;; Paragraphs
;; ======================================================================

;; Require whitespace after block starters so that paragraphs
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

;; Ordinary printable ASCII cannot be a newline, so a run of it is consumed by
;; a single range test per character.  Everything else (non-ASCII text, control
;; characters, the newline itself) falls through to the checked branch, which
;; is what actually decides where the line ends.  `#\~' is 126 and both
;; newline characters are below 32, so the range can never swallow one.
(define-peg-pattern line-content all
                    (* (or (range #\space #\~) "\t"
                           (and (not-followed-by nl-char) peg-any))))

;; =====================================================================
;; Inline Org Markup
;; =====================================================================

(define-peg-pattern escape-backslash none "\\")
(define-peg-pattern inline-escape-char all
                    (or "*"
                        "/"
                        "_"
                        "~"
                        "="
                        "+"
                        "^"
                        "{"
                        "}"
                        "["
                        "]"
                        "\\"))
(define-peg-pattern inline-escape body
                    (and escape-backslash inline-escape-char))

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
(define-inline-markup inline-strike inline-strike-delimiter inline-strike-body
                      inline-strike-content "+")

;; Superscript and subscript, in their unambiguous braced form
;; (`x^{2}` and `x_{i}`).  The bare forms (`x^2`) are intentionally not
;; recognised so that they never conflict with emphasis markers.
(define-peg-pattern mark-caret none "^")
(define-peg-pattern mark-underscore none "_")
(define-peg-pattern mark-brace-open none "{")
(define-peg-pattern mark-brace-close none "}")
(define-peg-pattern inline-superscript-content all
                    (+ (and (not-followed-by "}") peg-any)))
(define-peg-pattern inline-superscript body
                    (and mark-caret mark-brace-open inline-superscript-content
                         mark-brace-close))
(define-peg-pattern inline-subscript-content all
                    (+ (and (not-followed-by "}") peg-any)))
(define-peg-pattern inline-subscript body
                    (and mark-underscore mark-brace-open
                         inline-subscript-content mark-brace-close))

;; A forced line break is `\\' at the end of a line (trailing whitespace is
;; allowed).  Everywhere else `\\' is an escaped backslash, handled by
;; `inline-escape'.
(define-peg-pattern inline-line-break all
                    (and "\\\\"
                         (or (followed-by (+ ws))
                             (not-followed-by peg-any))))

;; Footnotes: inline `[fn::def]', reference `[fn:label]' and
;; definition `[fn:label:def]'.
(define-peg-pattern footnote-label all
                    (+ (and (not-followed-by (or "]" ":" nl)) peg-any)))
(define-peg-pattern footnote-content all
                    (* (and (not-followed-by "]") peg-any)))
(define-peg-pattern fn-open none "[fn:")
(define-peg-pattern fn-open-inline none "[fn::")
(define-peg-pattern fn-close none "]")
(define-peg-pattern fn-colon none ":")
(define-peg-pattern inline-footnote-inline all
                    (and fn-open-inline footnote-content fn-close))
(define-peg-pattern inline-footnote-def all
                    (and fn-open footnote-label fn-colon footnote-content
                         fn-close))
(define-peg-pattern inline-footnote-ref all
                    (and fn-open footnote-label fn-close))
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

;; The schemes are grouped by their first character so that a position that
;; cannot begin a scheme fails after a couple of one-character comparisons
;; instead of fifteen whole-string ones.
(define-peg-pattern url-scheme all
                    (or (and "h"
                             (or "ttps://" "ttp://"))
                        (and "f"
                             (or "tps://" "tp://" "ile://"))
                        (and "s"
                             (or "sh://" "ftp://"))
                        (and "w"
                             (or "ss://" "s://"))
                        (and "g"
                             (or "opher://" "it://"))
                        "tel:"
                        "mailto:"
                        (and "i"
                             (or "rcs://" "rc://"))))

(define-peg-pattern inline-raw-url all
                    (and url-scheme
                         (+ (and (not-followed-by (or ws nl)) peg-any))))

;; Ordered so the single-character markers are tried first and the multi-
;; character constructs (footnotes, links) and the scheme scan come last:
;; markup-heavy text then finds its markers without walking the whole list.
(define-peg-pattern inline-special body
                    (or inline-bold
                        inline-italic
                        inline-code
                        inline-verbatim
                        inline-underline
                        inline-strike
                        inline-superscript
                        inline-subscript
                        inline-line-break
                        inline-escape
                        inline-footnote-inline
                        inline-footnote-def
                        inline-footnote-ref
                        inline-link-with-desc
                        inline-link-no-desc
                        inline-raw-url))

;; Fast path for ordinary inline text.  `inline-special' can only start with
;; one of the markup characters (`* / ~ = + ^ _ \\ [') or with a letter that
;; can begin a URL scheme (h f s w g t m i r).  Every other character is known
;; to be plain, so a whole run of them costs a few range tests instead of one
;; `not-followed-by' check against the entire inline grammar per character.
;; The scheme-initial letters are deliberately left out of the fast path, so a
;; URL glued to the end of a word is still recognised.
(define-peg-pattern inline-plain-char body
                    (or (range #\a #\e)
                        (range #\j #\l)
                        (range #\n #\q)
                        (range #\u #\v)
                        (range #\x #\z)
                        (range #\A #\Z)
                        (range #\0 #\9)
                        " "
                        "\t"
                        "."
                        ","
                        ";"
                        ":"
                        "?"
                        "!"
                        "("
                        ")"
                        "-"
                        "'"
                        "\""
                        "%"
                        "&"
                        "@"
                        "#"
                        "{"
                        "}"
                        "<"
                        ">"
                        "|"
                        "$"))

(define-peg-pattern inline-text all
                    (+ (or inline-plain-char
                           (and (not-followed-by (or inline-special nl-char))
                                peg-any))))

(define-peg-pattern inline-element body
                    (or inline-special inline-text))
(define-peg-pattern inline-content body
                    (* inline-element))

;; =====================================================================
;; Helpers
;; =====================================================================

(define org-todo-keywords
  ;; The TODO keywords recognised at the start of a heading.  Org lets these be
  ;; customised per document; this is the common default set.
  '("TODO" "DONE"
    "NEXT"
    "WAIT"
    "WAITING"
    "HOLD"
    "HOLDING"
    "STARTED"
    "CANCELLED"
    "CANCELED"
    "IDEA"
    "NOTE"
    "FIXME"
    "PROJ"
    "IN-PROGRESS"))

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

(define (last-whitespace-index str)
  "Return the index of the last space or tab in @var{str}, or @code{#f}."
  (let loop
    ((i (- (string-length str) 1)))
    (cond
      ((< i 0)
       #f)
      ((or (char=? (string-ref str i) #\space)
           (char=? (string-ref str i) #\tab))
       i)
      (else (loop (- i 1))))))

(define (tag-character? c)
  "Return @code{#t} when @var{c} may appear inside an Org tag."
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c
            (list #\_ #\@ #\# #\% #\-))))

(define (parse-tags str)
  "Return the list of tags in @var{str} when it looks like @code{:a:b:}.
Returns @code{#f} otherwise, so a title that merely ends with a colon is not
mistaken for a tag group."
  (and (> (string-length str) 2)
       (char=? (string-ref str 0) #\:)
       (char=? (string-ref str
                           (- (string-length str) 1)) #\:)
       (let ((inner (substring str 1
                               (- (string-length str) 1))))
         (and (not (string-null? inner))
              (let ((parts (string-split inner #\:)))
                (and (every (lambda (part)
                              (and (not (string-null? part))
                                   (every tag-character?
                                          (string->list part)))) parts) parts))))))

(define (parse-heading text)
  "Split the text of a heading @var{text} into its structural parts.
Returns four values: the TODO keyword, the priority, the title proper and the
list of tags.  Any of the first two may be @code{#f} and the tag list may be
empty."
  (let* ((trimmed (trim-whitespace text))
         (tags (let ((wi (last-whitespace-index trimmed)))
                 (and wi
                      (parse-tags (substring trimmed
                                             (+ wi 1)
                                             (string-length trimmed))))))
         (body (if tags
                   (trim-whitespace (substring trimmed 0
                                               (last-whitespace-index trimmed)))
                   trimmed))
         (todo (let ((sp (string-index body #\space)))
                 (and sp
                      (let ((first (substring body 0 sp)))
                        (and (member first org-todo-keywords) first)))))
         (after-todo (if todo
                         (trim-whitespace (substring body
                                                     (string-length todo)))
                         body))
         (priority-close (and (>= (string-length after-todo) 3)
                              (string-prefix? "[#" after-todo)
                              (string-index after-todo #\])))
         (priority (and priority-close
                        (substring after-todo 2 priority-close)))
         (title (if priority
                    (trim-whitespace (substring after-todo
                                                (+ priority-close 1)))
                    after-todo)))
    (values todo priority title
            (or tags
                '()))))

(define (process-ast ast)
  "Recursively process the Abstract Syntax Tree @var{ast} generated by the PEG parser.
This function traverses the tree and restructures each flat @code{org-list}
into a properly nested list tree by invoking @code{build-list-tree}; the
resulting @code{nested-list} nodes hold items of any kind, so a nested list may
change its bullet type.  Headings are split into their TODO keyword, priority
and tags so renderers need not know about Org's heading syntax."
  (match ast
    (((quote org-list) . items) (cons 'org-list
                                      (car (build-list-tree items 0))))
    (('heading (? string? stars)
      ('line-content text))
     (call-with-values (lambda ()
                         (parse-heading text))
                       (lambda (todo priority title tags)
                         `(heading ,stars
                                   ,@(if todo
                                         (list `(heading-todo ,todo))
                                         '())
                                   ,@(if priority
                                         (list `(heading-priority ,priority))
                                         '())
                                   (line-content ,title)
                                   ,@(if (null? tags)
                                         '()
                                         (list (cons 'heading-tags tags)))))))
    ((node-type . children)
     ;; `children' is always a proper list here, so the old `(list? children)'
     ;; only walked it to no effect; `map' is what actually matters.
     (cons node-type
           (map process-ast children)))
    (other other)))

(define (get-indent item)
  "Calculate the indentation level of a parsed list @var{item}.
Returns the string length of the whitespace prefix if present,
or @code{0} if the item has no indentation."
  (match item
    ((_ (? string? spaces) . _) (string-length spaces))
    (_ 0)))

(define (strip-indent item)
  "Remove the indentation string from a parsed list @var{item}.
Returns a new pair containing the item's tag and its structural contents,
effectively stripping out the raw whitespace string."
  (match item
    ((tag (? string? spaces) . contents) (cons tag contents))
    (other other)))

(define (build-list-tree items depth)
  "Construct a hierarchical nested tree from a flat list of @var{items}.
Compares the indentation of each item against the current @var{depth}.
Items with an indentation greater than @var{depth} are recursively grouped
into @code{nested-list} nodes. Returns a pair consisting of the accumulated
tree and any remaining unparsed items."
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
;; Link Targets
;; =====================================================================
;;
;; An Org link is `[[target][description]]' (or `[[target]]'), and the target
;; says what kind of link it is: an external URL, a file (possibly an image), a
;; `#custom-id', a `*Heading' in the same document, or a plain path.  All of
;; that is decided here, once, so the HTML and Markdown renderers only have to
;; ask what they need instead of each re-reading the target themselves.

;; The extensions Org renders as an image when they appear in a `file:' link or
;; in a bare path.  Lower-case, without the leading dot.
(define org-image-extensions
  '("png" "jpg"
    "jpeg"
    "gif"
    "svg"
    "webp"
    "avif"
    "bmp"
    "tiff"
    "tif"
    "ico"))

(define (file-name path)
  "Return the last path component of @var{path}."
  (let ((slash (string-rindex path #\/)))
    (if slash
        (substring path
                   (+ slash 1)
                   (string-length path)) path)))

(define (file-extension path)
  "Return the lower-case extension of @var{path} without its dot, or @code{#f}.
A query string is ignored, so @code{photo.png?raw=1} is still a PNG."
  (let* ((name (file-name (car (string-split path #\?))))
         (dot (string-rindex name #\.)))
    (and dot
         (< 0 dot
            (- (string-length name) 1))
         (string-downcase (substring name
                                     (+ dot 1)
                                     (string-length name))))))

(define (url-scheme? str)
  "Return @code{#t} when @var{str} looks like a URL scheme such as `https'.
A scheme is a lower-case letter followed by lower-case letters, digits, `+',
`-' or `.'; insisting on lower case is what keeps the Windows path `C:/tmp'
from being read as a scheme called `c'."
  (let ((n (string-length str)))
    (and (> n 0)
         (char<=? #\a
                  (string-ref str 0) #\z)
         (every (lambda (c)
                  (or (char<=? #\a c #\z)
                      (char-numeric? c)
                      (memv c
                            '(#\+ #\- #\.))))
                (string->list (substring str 1 n))))))

(define (link-search-split target)
  "Split @var{target} at the first `::', giving @code{(path . search)}.
The search part is @code{#f} when the target has none; it is what Org uses for
`file:notes.org::*Heading' and `file:notes.org::#anchor'."
  (let ((at (string-contains target "::")))
    (if at
        (cons (substring target 0 at)
              (substring target
                         (+ at 2)
                         (string-length target)))
        (cons target #f))))

(define (org-link-parts target)
  "Split the Org link @var{target} into @code{(protocol . path)}.
@var{protocol} is a symbol:

@itemize
@item
@code{http}, @code{https}, @code{mailto}, @code{data} and friends, for a link
that carries a URL scheme of its own;
@item
@code{file} for a @code{file:} link and for a bare path such as @code{notes.org};
@item
@code{internal} for a @code{#custom-id} link;
@item
@code{heading} for a @code{*Heading} link.
@end itemize

@var{path} is @var{target} with its prefix removed and with any `::search'
suffix dropped, so it is ready to be used as an href or a file name.
@code{org-link-search} returns that suffix separately."
  (let ((path (car (link-search-split target))))
    (cond
      ((string-prefix? "#" path)
       (cons 'internal
             (substring path 1
                        (string-length path))))
      ((string-prefix? "*" path)
       (cons 'heading
             (substring path 1
                        (string-length path))))
      ((string-prefix? "file://" path)
       (cons 'file
             (substring path 7
                        (string-length path))))
      ((string-prefix? "file:" path)
       (cons 'file
             (substring path 5
                        (string-length path))))
      (else (let ((colon (string-index path #\:)))
              (if (and colon
                       (> colon 0)
                       (url-scheme? (substring path 0 colon)))
                  (cons (string->symbol (string-downcase (substring path 0
                                                                    colon)))
                        (substring path
                                   (+ colon 1)
                                   (string-length path)))
                  (cons 'file path)))))))

(define (org-link-protocol target)
  "Return the protocol symbol of the Org link @var{target}.
See @code{org-link-parts} for the possible values."
  (car (org-link-parts target)))

(define (org-link-path target)
  "Return the path of the Org link @var{target}, without protocol or search part."
  (cdr (org-link-parts target)))

(define (org-link-search target)
  "Return the `::search' part of the Org link @var{target}, or @code{#f}.
This is the @code{*Heading'} or @code{#anchor} that a file link points inside
the linked document; it is not something the renderers act on yet."
  (cdr (link-search-split target)))

(define (org-image-link? target)
  "Return @code{#t} when the Org link @var{target} should render as an image.
A `file:' link (or a bare path) whose extension is one of
@code{org-image-extensions} counts, as does a @code{data:image/...} URI."
  (let ((protocol (org-link-protocol target))
        (path (org-link-path target)))
    (cond
      ((eq? protocol
            'file)
       (let ((extension (file-extension path)))
         (and extension
              (member extension org-image-extensions) #t)))
      ((eq? protocol
            'data)
       (string-prefix? "image/" path))
      (else #f))))

(define (org-link-href target)
  "Return the href an Org link @var{target} should point at.
File links give their path, `#custom-id' and `*Heading' links give a local
fragment, and a link with a scheme of its own keeps it."
  (let ((protocol (org-link-protocol target))
        (path (org-link-path target)))
    (cond
      ((eq? protocol
            'internal)
       (string-append "#" path))
      ((eq? protocol
            'heading)
       (string-append "#"
                      (org-anchor path)))
      ((eq? protocol
            'file)
       path)
      (else (format #f "~a:~a" protocol path)))))

(define (org-link-display target)
  "Return the text Org shows for a link @var{target} that has no description.
An internal or heading link shows its id or title, a file link its path, and a
link with a scheme of its own the full URL."
  (let ((protocol (org-link-protocol target)))
    (if (or (eq? protocol
                 'internal)
            (eq? protocol
                 'heading))
        (org-link-path target)
        (org-link-href target))))

(define (org-anchor text)
  "Return a URL-fragment-safe anchor for @var{text}.
Lower-case letters and digits survive, every run of anything else becomes a
single dash, and leading or trailing dashes are dropped: `My First Section'
becomes @code{my-first-section}.  Headings carry this as their @code{id} and
`*Heading' links point at it, so the two agree by construction."
  (let loop
    ((chars (string->list (string-downcase text)))
     (out '())
     (dash? #t))
    (if (null? chars)
        (list->string (reverse (if (and (pair? out)
                                        (char=? (car out) #\-))
                                   (cdr out) out)))
        (let ((c (car chars)))
          (cond
            ((or (char-alphabetic? c)
                 (char-numeric? c))
             (loop (cdr chars)
                   (cons c out) #f))
            (dash? (loop (cdr chars) out #t))
            (else (loop (cdr chars)
                        (cons #\- out) #t)))))))

;; =====================================================================
;; Parser Entry Point
;; =====================================================================

(define (inline-org->ast text)
  "Parse inline Org-mode markup from @var{text}.
Applies the @code{inline-content} PEG pattern to process bold, italic,
raw URLs, inline code, and links, returning the resulting AST."
  (peg:tree (match-pattern inline-content text)))

(define (org->ast str)
  "Parse an entire Org-mode document string @var{str} into an Abstract Syntax Tree.
This is the main entry point for the parser. It matches the @code{org-doc}
PEG pattern and then applies @code{process-ast} to resolve hierarchical
constructs like nested lists.  A single top-level element comes back as that
element's node; several come back as a list of nodes."
  (let ((tree (peg:tree (match-pattern org-doc str))))
    (if (and (pair? tree)
             (symbol? (car tree)))
        (process-ast tree)
        (map process-ast tree))))
