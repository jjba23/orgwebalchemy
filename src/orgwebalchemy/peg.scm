;; orgwebalchemy
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The grammar and the two entry points every renderer reads through: a whole
;; Org document becomes a tree with `org->ast', and a string of inline Org
;; markup becomes a tree with `inline-org->ast'.  Nothing here renders: the
;; module knows the language, the renderers know the output.
;;
;; ```mermaid
;; flowchart LR
;;   org["Org text"] --> doc["org->ast<br/>block grammar"]
;;   inline["inline text"] --> inline-ast["inline-org->ast<br/>inline grammar"]
;;   doc --> tree["an AST of tagged lists"]
;;   inline-ast --> tree
;;   tree --> html["(orgwebalchemy html)"]
;;   tree --> markdown["(orgwebalchemy markdown)"]
;;   tree --> json["(orgwebalchemy json)"]
;; ```
;;
;; The links, figures and named elements are classified once, here, so the
;; renderers cannot disagree about what a target means.

;;; Code:

(define-module (orgwebalchemy peg)
  #:declarative? #t
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (org->ast inline-org->ast
                     figure-parts
                     named-parts
                     document-names
                     element-names
                     org-link-parts
                     org-link-protocol
                     org-link-path
                     org-link-search
                     org-link-href
                     org-link-display
                     link-protocols
                     org-image-link?
                     org-anchor
                     heading-parts
                     inline-nodes
                     split-checkbox
                     list-item-kind
                     group-list-items
                     trim-whitespace))

;; =====================================================================
;; Macros for PEG Pattern Generation
;; =====================================================================

;; A block is a begin marker, its content, an end marker and the end of the
;; line.  The content is everything up to the end marker, it reaches the tree
;; as one string, and it carries the tag `define-peg-pattern' would have put on
;; it -- an empty string when the block is empty, so that an empty block is
;; still the block it is, rather than a bare tag with nothing under it.
(define-syntax-rule (define-block-pattern name begin-pat end-pat content-pat)
  (begin
    (define (content-pat str len pos)
      (let ((end (run-end str len pos raw-stop-chars end-pat)))
        (list end
              (list 'content-pat
                    (substring str pos end)))))
    (define-peg-pattern name all
                        (and begin-pat
                             eol
                             content-pat
                             end-pat
                             (* ws)
                             eol))))

;; A markup construct is a delimiter, its body and the delimiter again.  The
;; delimiter has to be a single character, so that the body can be found with
;; one search; the body is a run scanner, not a character-by-character match.
(define-syntax-rule (define-inline-markup name name-scan name-delim
                                          name-content delim)
  (begin
    (define-peg-pattern name-delim none delim)
    (define name-scan
      (let ((delimiter (string-ref delim 0)))
        (lambda (str len pos)
          "Consume the body of the markup construct as one string."
          (text-run+ str len pos
                     (markup-content-candidates delimiter)
                     (markup-content-stop? delimiter)))))
    (define-peg-pattern name-content all name-scan)
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
;; Run Scanners
;; =====================================================================
;;
;; `(ice-9 peg)' compiles `(* pattern)' and `(+ pattern)' into a loop that
;; calls the inner pattern once per repetition and keeps one parse object per
;; iteration.  Written that way, a scanner hands the tree one fragment per
;; character, and the fragments then have to be merged back into the string
;; they spell: a line of `n' characters costs `n' one-character substrings, `n'
;; list cells, and a merge pass over all of them.
;;
;; A compiled pattern is only a procedure of `(string length position)' that
;; returns `(end . body)', or `#f', so a scanner that consumes a whole run at
;; once can be written here and named in the grammar like any other pattern.
;; `string-index' finds the end of a run in a single C-level scan, and the run
;; then becomes one substring: a fragment per run rather than per character,
;; and nothing left for the collapse pass to merge.
;;
;; Each scanner returns the body the `(* ...)' or `(+ ...)' it replaces would
;; have produced after collapsing -- a string, or `'()' for an empty match --
;; and `define-peg-pattern' attaches the same node tag it attached before.
;; `inline-text-scanner' calls `inline-special', which is defined with the
;; inline grammar below.

(define nl-chars
  ;; The characters that end a line of text.
  (char-set #\newline #\return))

(define table-stop-chars
  ;; A table cell ends at the cell separator and at the end of the line.
  (char-set #\| #\newline #\return))

(define inline-stop-chars
  ;; Every character that can begin an inline construct: the markup markers
  ;; (`*' bold, `/' italic, `~' code, `=' verbatim, `_' underline and
  ;; subscript, `+' strike, `^' superscript, `\' escape, `[' footnotes and
  ;; links), the letters a raw URL scheme can start with (`h f s w g t m i') and
  ;; the newline.  `r' cannot begin anything; it is here only because the extra
  ;; `inline-special' check it costs lands on a position that can never begin a
  ;; construct.  Anything else in a line is known to be plain, and a run of it
  ;; is taken without a single guess.
  (char-set #\*
            #\/
            #\~
            #\=
            #\_
            #\+
            #\^
            #\\
            #\[
            #\h
            #\f
            #\s
            #\w
            #\g
            #\t
            #\m
            #\i
            #\r
            #\newline
            #\return))

(define keyword-key-chars
  ;; A key ends at whitespace, at the colon that separates it from the value,
  ;; and at the end of the line.
  (char-set #\space #\tab #\: #\newline #\return))

(define raw-stop-chars
  ;; Every end marker a block can have begins with a `#`.
  (char-set #\#))

(define whitespace-chars
  ;; A run of text ends at whitespace, and at the end of the line.
  (char-set #\space #\tab #\newline #\return))

(define brace-chars
  ;; Superscripts and subscripts end at the closing brace.
  (char-set #\}))

(define label-stop-chars
  ;; A footnote label ends at `]`, at the `:` before a definition, and at the
  ;; end of the line.
  (char-set #\] #\: #\newline #\return))

(define close-bracket-chars
  ;; A footnote body ends at `]`, newlines and all.
  (char-set #\]))

(define link-stop-chars
  ;; A link's target and description end at `]` and at the end of the line.
  (char-set #\] #\newline #\return))

(define (run-end str len pos stop-chars stop?)
  "Return the end of the run of @var{str} from @var{pos} that stops at the
first character of @var{stop-chars} at which @var{stop?} holds, or at
@var{len}.  @var{stop?} is given the string, its length and the position of the
candidate, so a candidate that turns out not to end the run is taken as content
and the scan goes on after it."
  (let loop
    ((i pos))
    (let ((candidate (or (string-index str stop-chars i len) len)))
      (if (or (>= candidate len)
              (stop? str len candidate)) candidate
          (loop (1+ candidate))))))

(define (always-stop str len i)
  "Stop at every candidate: a run ends at the first character of its set." #t)

(define (lone-carriage-return? str len i)
  "Return @code{#t} when the candidate at @var{i} is a carriage return that does
not end a line, because no newline follows it."
  (and (char=? (string-ref str i) #\return)
       (or (>= (+ i 1) len)
           (not (char=? (string-ref str
                                    (+ i 1)) #\newline)))))

(define (line-end? str len i)
  "Return @code{#t} for every candidate but a lone carriage return.
This is for stop sets that already hold nothing but the characters that end the
construct -- the newlines, `]' and `:' of a footnote, `|' of a table cell -- so
the only question left is whether a carriage return really ends a line."
  (not (lone-carriage-return? str len i)))

(define (nl-end? str len i)
  "Return @code{#t} when the candidate at @var{i} ends a line.
That is a newline, or a carriage return with a newline after it.  Use this where
the candidate set holds characters that do not end a line at all, such as the
space and tab of a description key."
  (let ((c (string-ref str i)))
    (or (char=? c #\newline)
        (and (char=? c #\return)
             (< (+ i 1) len)
             (char=? (string-ref str
                                 (+ i 1)) #\newline)))))

(define (text-run str len pos stop-chars stop?)
  "Consume the run of @var{str} from @var{pos} that stops at the first
character of @var{stop-chars} that @var{stop?} accepts, and return it as one
string.  An empty run succeeds, as the empty string.

Returning the empty string rather than the empty match a `(* ...)' produces is
what keeps an empty value in its place: a pattern that accumulates `all' turns
an empty match into the bare tag symbol, so a table cell with nothing between
its separators would drop its column out of the row, and a source block written
without a language would vanish from the document.  The text of something may
be empty; it is still text."
  (let ((end (run-end str len pos stop-chars stop?)))
    (list end
          (substring str pos end))))

(define (text-run+ str len pos stop-chars stop?)
  "Like @code{text-run}, but an empty run fails, as a `(+ ...)' match did."
  (let ((end (run-end str len pos stop-chars stop?)))
    (and (> end pos)
         (list end
               (substring str pos end)))))

(define (raw-run str len pos end-pat)
  "Consume everything up to the first @var{end-pat} match as one string.
This is what a block's content is: the body a
@code{(* (and (not-followed-by END-PAT) peg-any))} would have produced, in one
piece."
  (text-run str len pos raw-stop-chars end-pat))

(define (inline-end? str len i)
  "Return @code{#t} when the candidate at @var{i} ends a run of inline text.
That is the end of the line, or an inline construct that really begins here;
a character that could begin one but does not is plain text."
  (let ((c (string-ref str i)))
    (or (char=? c #\newline)
        (char=? c #\return)
        (inline-special str len i))))

(define (link-target-end? str len i)
  "Return @code{#t} when the candidate at @var{i} ends a link target.
A `]' does so only when it closes the link or opens its description, which is
what the link patterns themselves decide."
  (if (char=? (string-ref str i) #\])
      (or (link-close str len i)
          (link-sep str len i))
      (line-end? str len i)))

(define (link-description-end? str len i)
  "Return @code{#t} when the candidate at @var{i} ends a link's description.
Inside a description a `][' is ordinary text; only `]]' closes it."
  (if (char=? (string-ref str i) #\])
      (link-close str len i)
      (line-end? str len i)))

(define (whitespace-end? str len i)
  "Return @code{#t} when the candidate at @var{i} is whitespace or a line end."
  (let ((c (string-ref str i)))
    (or (char=? c #\space)
        (char=? c #\tab)
        (nl-end? str len i))))

(define (desc-key-end? str len i)
  "Return @code{#t} when the candidate at @var{i} ends a description key.
That is the end of the line, or the whitespace run before the `::' that
separates the key from the detail -- a decision the `desc-sep' pattern makes."
  (or (nl-end? str len i)
      (and (desc-sep str len i) #t)))

;; The text of a paragraph line or a heading title: anything up to the newline.
(define (line-text-scanner str len pos)
  "Consume the rest of the line as one string."
  (text-run str len pos nl-chars always-stop))

;; The text of one table cell: anything up to the cell separator or the newline.
(define (table-cell-scanner str len pos)
  "Consume the rest of one table cell as one string."
  (text-run str len pos table-stop-chars line-end?))

;; The text a construct carries on its own line: a keyword's value, a source
;; block's language, the text of a comment or a fixed-width line.
(define (rest-of-line-scanner str len pos)
  "Consume the rest of a line of a construct's own text as one string."
  (text-run str len pos nl-chars line-end?))

(define (inline-text-scanner str len pos)
  "Consume one run of plain inline text, and return it as one string.
Fails when there is nothing to consume, as the `(+ ...)' this replaces did."
  (text-run+ str len pos inline-stop-chars inline-end?))

(define (brace-content-scanner str len pos)
  "Consume a superscript's or subscript's text, up to its closing brace."
  (text-run+ str len pos brace-chars always-stop))

(define (footnote-label-scanner str len pos)
  "Consume a footnote's label, as one string."
  (text-run+ str len pos label-stop-chars line-end?))

(define (footnote-content-scanner str len pos)
  "Consume a footnote's text, up to the closing bracket, as one string."
  (text-run str len pos close-bracket-chars always-stop))

(define (link-target-scanner str len pos)
  "Consume a link's target, up to `]]', `][' or the end of the line."
  (text-run+ str len pos link-stop-chars link-target-end?))

(define (link-description-scanner str len pos)
  "Consume a link's description, up to `]]' or the end of the line."
  (text-run+ str len pos link-stop-chars link-description-end?))

(define (raw-url-body-scanner str len pos)
  "Consume the body of a raw URL, up to whitespace or the end of the line."
  (text-run+ str len pos whitespace-chars whitespace-end?))

(define (desc-key-scanner str len pos)
  "Consume the key of a description item, up to `::' or the end of the line."
  (text-run+ str len pos whitespace-chars desc-key-end?))

(define (keyword-key-scanner str len pos)
  "Consume a keyword's key, as one string."
  (text-run+ str len pos keyword-key-chars line-end?))

(define (src-content-scanner str len pos)
  "Consume a source block's content, up to its end marker, as one string."
  (raw-run str len pos end-src))

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
;; One table cell, scanned as one run rather than a character at a time; see
;; the Run Scanners section above.
(define-peg-pattern table-cell all table-cell-scanner)

;; The end of a row: its closing bar, or nothing at all before the end of the
;; line, since Org lets the last bar be left out.
(define-peg-pattern table-row-end none
                    (or (and table-sep
                             (* ws) eol)
                        (and (* ws) eol)))

;; A row holds one or more cells.  Each of them is written after a bar, and a
;; bar separates two cells unless it closes the row -- unless nothing but the
;; end of the line follows it.  That test is what makes `| a | b |' a row of two
;; cells, with the third bar closing it, while `| a ||' has an empty second
;; cell.  It stands before the bar rather than after it because
;; `(not-followed-by ...)' only sees a match that consumes something, and at the
;; end of the input the end of a line is a match of nothing at all.
;;
;; A line that is nothing but its closing bar is the one case the test cannot
;; see, because a row has to open with a bar, so the second alternative covers
;; it and gives the one empty cell Org reads such a line as.
(define-peg-pattern table-row-cells body
                    (or (and (followed-by table-sep)
                             (+ (and (not-followed-by (and table-sep
                                                           (* ws) eol))
                                     table-sep table-cell)) table-row-end)
                        (and table-sep table-cell table-row-end)))

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
(define-peg-pattern desc-key all desc-key-scanner)
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

(define (markup-content-candidates delimiter)
  "Return the characters at which the body of a markup construct can end:
its delimiter, the end of the line, and the whitespace a delimiter or a line
end could follow."
  (char-set delimiter #\space #\tab #\newline #\return))

(define (markup-content-stop? delimiter)
  "Return the predicate that ends a markup construct's body at the candidate it
is given.  The body stops at DELIMITER itself, at the end of the line, and at
whitespace the closing delimiter or a line end follows -- which is what the
`name-body' pattern decided: a space before the closing delimiter is not part
of the body, so `*bold *' is not bold, while `*bo ld*' is."
  (lambda (str len i)
    (let ((c (string-ref str i)))
      (cond
        ((char=? c delimiter)
         #t)
        ((or (char=? c #\space)
             (char=? c #\tab))
         (and (< (+ i 1) len)
              (or (char=? (string-ref str
                                      (+ i 1)) delimiter)
                  (nl-end? str len
                           (+ i 1)))))
        (else (nl-end? str len i))))))

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
;; The language is the rest of the `#+BEGIN_SRC' line; a block written without
;; one has an empty language, not no language, so the block still renders.
(define-peg-pattern lang all rest-of-line-scanner)

;; The marker has to be followed by whitespace or the end of the line, so
;; `#+BEGIN_SRCfoo' is not a block; the language itself may then be empty.
(define-peg-pattern src-block all
                    (and begin-src
                         (or (+ ws)
                             (followed-by nl))
                         lang
                         nl
                         src-content
                         end-src
                         (* ws)
                         eol))
(define-peg-pattern src-content all src-content-scanner)

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
(define-peg-pattern keyword-key all keyword-key-scanner)
(define-peg-pattern keyword-value all rest-of-line-scanner)
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
(define-peg-pattern comment-content all rest-of-line-scanner)
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
(define-peg-pattern fixed-width-content all rest-of-line-scanner)
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

;; A whole line of text, scanned as one run rather than a character at a time;
;; see the Run Scanners section above.
(define-peg-pattern line-content all line-text-scanner)

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

(define-inline-markup inline-code inline-code-body-run inline-code-delimiter
                      inline-code-content "~")
(define-inline-markup inline-verbatim inline-verbatim-body-run
                      inline-verbatim-delimiter inline-verbatim-content "=")
(define-inline-markup inline-italic inline-italic-body-run
                      inline-italic-delimiter inline-italic-content "/")
(define-inline-markup inline-bold inline-bold-body-run inline-bold-delimiter
                      inline-bold-content "*")
(define-inline-markup inline-underline inline-underline-body-run
                      inline-underline-delimiter inline-underline-content "_")
(define-inline-markup inline-strike inline-strike-body-run
                      inline-strike-delimiter inline-strike-content "+")

;; Superscript and subscript, in their unambiguous braced form
;; (`x^{2}` and `x_{i}`).  The bare forms (`x^2`) are intentionally not
;; recognised so that they never conflict with emphasis markers.
(define-peg-pattern mark-caret none "^")
(define-peg-pattern mark-underscore none "_")
(define-peg-pattern mark-brace-open none "{")
(define-peg-pattern mark-brace-close none "}")
(define-peg-pattern inline-superscript-content all brace-content-scanner)
(define-peg-pattern inline-superscript body
                    (and mark-caret mark-brace-open inline-superscript-content
                         mark-brace-close))
(define-peg-pattern inline-subscript-content all brace-content-scanner)
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
(define-peg-pattern footnote-label all footnote-label-scanner)
(define-peg-pattern footnote-content all footnote-content-scanner)
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
(define-peg-pattern link-target-content all link-target-scanner)
(define-peg-pattern link-description-content all link-description-scanner)

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
                    (and url-scheme raw-url-body-scanner))

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

;; A whole run of ordinary inline text, scanned as one run rather than a
;; character at a time; see the Run Scanners section above.  The scanner stops
;; at a character that can begin a construct only once `inline-special' agrees
;; that one really does, so a URL glued to the end of a word is still found.
(define-peg-pattern inline-text all inline-text-scanner)

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

;; =====================================================================
;; Reading the Tree
;; =====================================================================
;;
;; Both renderers walk the same tree and have to read the same things out of
;; it -- the parts of a heading, the kind of a list item, whether an item is a
;; checkbox.  They are read here, next to the grammar that produced them, so
;; that the renderers only have to decide what to write and the two cannot
;; disagree about what the tree means.

(define (heading-parts children)
  "Return @code{(todo priority tags text)} for the children of a heading.
A heading is split into these by @code{process-ast}; anything it does not have
comes back as @code{#f}, except the tags, which are the empty list."
  (list (any (lambda (child)
               (match child
                 (('heading-todo todo)
                  todo)
                 (_ #f))) children)
        (any (lambda (child)
               (match child
                 (('heading-priority priority)
                  priority)
                 (_ #f))) children)
        (append-map (lambda (child)
                      (match child
                        (('heading-tags tags ...)
                         tags)
                        (_ '()))) children)
        (any (lambda (child)
               (match child
                 (('line-content text)
                  text)
                 (_ #f))) children)))

(define (inline-nodes ast)
  "Return @var{ast} as a list of nodes, whatever shape it came in.
A parser hands back one node, a list of nodes or nothing at all, depending on
what it matched; this is the one place that shape is flattened."
  (cond
    ((string? ast)
     (list ast))
    ((and (pair? ast)
          (symbol? (car ast)))
     (list ast))
    ((list? ast)
     ast)
    (else '())))

(define (split-checkbox text)
  "Return @code{(state . rest)} when @var{text} starts with an Org checkbox.
@var{state} is @code{#t} for `[X] '/`[x] ' and @code{#f} for `[ ] '; @var{rest}
is the item text with the checkbox removed.  Returns @code{#f} when @var{text}
is not a checkbox item."
  (match (string-match "^\\[([ xX])\\][[:space:]]+" text)
    (#f #f)
    (m (cons (let ((c (match:substring m 1)))
               (or (string=? c "x")
                   (string=? c "X")))
             (substring text
                        (match:end m 0))))))

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
runs, preserving their original order.  One Org list may mix unordered, ordered
and description items, so each run becomes a list element of its own."
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
;; The `#+NAME:' prefixes that name something *inside* this document, so a link
;; to one is a fragment rather than a URL of its own.  A figure is the one the
;; parser anchors, so `fig:' is the one that resolves; the rest are listed so
;; that they are at least classified as the cross-references they are.
(define org-cross-reference-prefixes
  '("fig:" "tbl:" "tab:" "lst:" "eq:"))

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
      ((any (lambda (prefix)
              (string-prefix? prefix path)) org-cross-reference-prefixes)
       (cons 'internal path))
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

;; The link protocols a host application knows.  The parser classifies a target
;; and stops there: whatever lowercase scheme a target spells becomes the symbol
;; of that scheme, and no scheme orgwebalchemy does not define itself is named
;; in this module.  A host that knows what `yt:' or `zotero:' means binds this
;; to an alist of protocol symbols and resolver procedures, and the resolver --
;; which takes the path and returns the href to use, or `#f' to decline the
;; target -- is left with the host, where it belongs.
(define link-protocols
  (make-parameter '()))

(define (custom-link-href protocol path)
  "Return the href a registered resolver gives @var{path} for @var{protocol}.
@code{#f} when no resolver is registered for @var{protocol}, or when the one
registered declined the target."
  (let ((entry (assq protocol
                     (link-protocols))))
    (and entry
         ((cdr entry) path))))

(define (org-link-href target)
  "Return the href an Org link @var{target} should point at.
A resolver bound in @code{link-protocols} decides first, for any protocol.  On
its own, a file link gives its path, `#custom-id' and `*Heading' links give a
local fragment, and a link with a scheme of its own keeps it."
  (let ((protocol (org-link-protocol target))
        (path (org-link-path target)))
    (or (custom-link-href protocol path)
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
          (else (format #f "~a:~a" protocol path))))))

(define (org-link-display target)
  "Return the text Org shows for a link @var{target} that has no description.
An internal or heading link shows its id or title, a file link its path, and a
link with a scheme of its own the full URL.  A target a resolver rewrote shows
what the document wrote instead, because `[[yt:dQw4w9WgXcQ]]' says exactly
that, and the href is where the resolution belongs."
  (let* ((protocol (org-link-protocol target))
         (path (org-link-path target)))
    (cond
      ((or (eq? protocol
                'internal)
           (eq? protocol
                'heading))
       path)
      ((custom-link-href protocol path)
       (car (link-search-split target)))
      (else (org-link-href target)))))

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
;; Figures
;; =====================================================================
;;
;; Org gives the element that follows `#+CAPTION:' a caption and `#+NAME:' a
;; name.  When that element is a link to an image, the caption, the name and the
;; image are one thing: a figure.  They are folded together here, once, so the
;; renderers only have to wrap what they are handed -- and so the name a
;; `[[fig:arch]]' link points at is the very id the figure carries.

(define (keyword-value-of node key)
  "Return the value of @var{node} when it is the keyword @var{key}, else @code{#f}.
Keyword names are compared case-insensitively, as Org does, and surrounding
whitespace is stripped; a keyword with an empty value counts as no value at
all, so `#+NAME:' on its own stays an ordinary keyword line."
  (match node
    (('keyword ('keyword-key name)
      ('keyword-value value))
     (and (string? name)
          (string-ci=? name key)
          (let ((trimmed (trim-whitespace value)))
            (and (not (string-null? trimmed)) trimmed))))
    (_ #f)))

(define (line-image-link text)
  "Return @code{(target . description)} when @var{text} is exactly one image link.
Surrounding whitespace is ignored and @var{description} is @code{#f} when the
link has none; a line that holds anything else returns @code{#f}."
  (let ((nodes (remove (lambda (node)
                         (and (string? node)
                              (string-null? (trim-whitespace node))))
                       (inline-nodes (inline-org->ast text)))))
    (match nodes
      ((('inline-link-with-desc ('link-target-content target)
         ('link-description-content description)))
       (and (org-image-link? target)
            (cons target description)))
      ((('inline-link-no-desc ('link-target-content target)))
       (and (org-image-link? target)
            (cons target #f)))
      (_ #f))))

(define (paragraph-image-link node)
  "Return @code{(target . description)} when @var{node} is one image link."
  (match node
    (('paragraph ('line-content text))
     (line-image-link text))
    (_ #f)))

(define (keyword-key? node)
  "Return @code{#t} when @var{node} is a `#+CAPTION:' or `#+NAME:' keyword."
  (and (pair? node)
       (or (keyword-value-of node "NAME")
           (keyword-value-of node "CAPTION")) #t))

(define (make-figure name caption image)
  "Build the figure node for @var{name}, @var{caption} and the image pair @var{image}.
What a figure does not have is left out rather than set to @code{#f}, so a
renderer can tell `no caption' from `an empty caption'."
  `(figure ,@(if name
                 (list (list 'figure-name name))
                 '())
           ,@(if caption
                 (list (list 'figure-caption caption))
                 '())
           (figure-image (figure-target ,(car image))
                         ,@(if (cdr image)
                               (list (list 'figure-description
                                           (cdr image)))
                               '()))))

(define (pending-value pending key)
  "Return the first value of keyword @var{key} among @var{pending}, or @code{#f}."
  (any (lambda (node)
         (keyword-value-of node key))
       (reverse pending)))

(define (pending-caption pending)
  "Return the captions among @var{pending} joined with a space, or @code{#f}.
Org allows one caption line per `#+CAPTION:' and a figure's caption is all of
them, in order."
  (let ((captions (filter-map (lambda (node)
                                (keyword-value-of node "CAPTION"))
                              (reverse pending))))
    (and (pair? captions)
         (string-join captions " "))))

;; The elements a `#+NAME:' can name: the ones that render as something a link
;; can land on.  A name in front of anything else stays an ordinary keyword
;; line, so a name is never attached to an element that has nowhere to carry it.
(define namable-elements
  '(paragraph table
              src-block
              example-block
              quote-block
              verse-block
              center-block
              export-html-block
              org-list
              nested-list
              separator))

(define (namable-element? node)
  "Return @code{#t} when @var{node} is an element a `#+NAME:' can name."
  (and (pair? node)
       (memq (car node) namable-elements)))

(define (make-named name node)
  "Build the node for the element @var{node} that `#+NAME:' calls @var{name}.
The name is the element's id, so a `[[tbl:data]]' or `[[name]]' link -- which
is classified as a local fragment -- lands on the element itself."
  `(named (element-name ,name)
          ,node))

(define (named-parts node)
  "Return @code{(name element)} for the named element @var{node}."
  (list (child-value node
                     'element-name)
        (and (pair? (cdr node))
             (caddr node))))

(define (flush-keywords pending out)
  "Put the keyword run @var{pending} -- held newest first -- back into @var{out},
which is building the result newest first, so the run keeps document order."
  (append pending out))

(define (without-name-keywords pending)
  "Return the keyword run @var{pending}, newest first, minus its `#+NAME:' lines.
A `#+NAME:' that named an element is consumed by it, exactly as it is by a
figure; a `#+CAPTION:' describes something this parser does not caption, so it
stays a keyword line where the writer put it."
  (filter (lambda (node)
            (not (keyword-value-of node "NAME"))) pending))

(define (collect-named-elements nodes)
  "Fold `#+NAME:' -- and, for an image, `#+CAPTION:' -- into what it describes.
A run of `#+CAPTION:' and `#+NAME:' keywords immediately before a paragraph
holding a single image link becomes one @code{figure} node.  Before any other
namable element a `#+NAME:' becomes a @code{named} node wrapping it, so the
name is the id that element carries and a cross-reference to the name resolves.
Keywords that describe something else keep their place, in document order."
  (let loop
    ((nodes nodes)
     (pending '())
     (out '()))
    (cond
      ((null? nodes)
       (reverse (flush-keywords pending out)))
      (else (let ((node (car nodes)))
              (if (keyword-key? node)
                  (loop (cdr nodes)
                        (cons node pending) out)
                  (let ((image (and (pair? pending)
                                    (paragraph-image-link node)))
                        (name (and (pair? pending)
                                   (pending-value pending "NAME"))))
                    (cond
                      (image (loop (cdr nodes)
                                   '()
                                   (cons (make-figure (pending-value pending
                                                                     "NAME")
                                                      (pending-caption pending)
                                                      image) out)))
                      ((and name
                            (namable-element? node))
                       (loop (cdr nodes)
                             '()
                             (cons (make-named name node)
                                   (flush-keywords (without-name-keywords
                                                    pending) out))))
                      (else (loop (cdr nodes)
                                  '()
                                  (cons node
                                        (flush-keywords pending out))))))))))))

(define (node-names node)
  "Return every name @var{node} and its children carry as a `figure-name' or an
`element-name', in document order.  This is what a link may resolve against."
  (cond
    ((not (pair? node))
     '())
    ((and (or (eq? (car node)
                   'figure-name)
              (eq? (car node)
                   'element-name))
          (string? (cadr node)))
     (cons (cadr node)
           (append-map node-names
                       (cddr node))))
    (else (append-map node-names
                      (cdr node)))))

(define (document-names nodes)
  "Return the names the elements of the parsed document @var{nodes} carry."
  (delete-duplicates (append-map node-names
                                 (if (and (pair? nodes)
                                          (symbol? (car nodes)))
                                     (list nodes) nodes))))

;; The names the elements of the document being parsed carry.  The inline
;; parser resolves a bare link target against them, and the renderers bind this
;; for the length of one document, so `[[name]]' finds the element that
;; `#+NAME: name' names; anywhere else the list is empty and a target is left
;; exactly as it was written.
(define element-names
  (make-parameter '()))

(define (name-link-target target names)
  "Return the fragment @var{target} should point at, when a name makes it one.
A target that is exactly the name of an element in this document is a
cross-reference, as it is in Org -- `[[diagram]]' finds `#+NAME: diagram' -- so
it becomes the fragment `#diagram'.  A target that says what it is -- it has a
scheme, a `file:' prefix, a leading `#' or `*', or a `::search' of its own -- is
left alone; only a bare target is resolved, and only when the name exists."
  (and (string? target)
       (member target names)
       (eq? (org-link-protocol target)
            'file)
       (not (org-link-search target))
       (string-append "#" target)))

(define (resolve-name-links node names)
  "Rewrite the links of @var{node} that `#+NAME:' targets name.
An inline parse comes back with the links the line wrote; this is the pass that
makes a bare `[[name]]' point at the element named @var{name} instead of at a
file that is not there.  A document's inline text is parsed as it is rendered,
so `org->ast' records the names and the renderers bind them here, by parsing
within @code{(element-names ...)}."
  (cond
    ((string? node)
     node)
    ((pair? node)
     (if (eq? (car node)
              'link-target-content)
         (let* ((target (and (pair? (cdr node))
                             (cadr node)))
                (fragment (name-link-target target names)))
           (if fragment
               (list 'link-target-content fragment) node))
         (cons (car node)
               (map (lambda (child)
                      (resolve-name-links child names))
                    (cdr node)))))
    (else node)))

(define (figure-parts node)
  "Return @code{(name caption target description)} for the figure @var{node}.
Every part the figure does not have comes back as @code{#f}."
  (let ((image (child-of node
                         'figure-image)))
    (list (child-value node
                       'figure-name)
          (child-value node
                       'figure-caption)
          (child-value image
                       'figure-target)
          (child-value image
                       'figure-description))))

(define (child-of node key)
  "Return the first child of @var{node} whose head is @var{key}, or @code{#f}.
This is the whole child, for a child that is itself a node -- the difference
between the @code{figure-image} of a figure and the value of a plain
@code{(key value)} pair, which @code{child-value} returns."
  (and (pair? node)
       (any (lambda (child)
              (and (pair? child)
                   (eq? (car child) key) child))
            (cdr node))))

(define (child-value node key)
  "Return the value of the first @code{(key value)} child of @var{node}, or @code{#f}."
  (let ((child (child-of node key)))
    (and child
         (cadr child))))

;; =====================================================================
;; Parser Entry Point
;; =====================================================================

(define (inline-org->ast text)
  "Parse inline Org-mode markup from @var{text}.
Applies the @code{inline-content} PEG pattern to process bold, italic,
raw URLs, inline code, and links, returning the resulting AST.  A link whose
bare target names an element of the document being rendered resolves to that
element; see @code{element-names}."
  (resolve-name-links (parse-with inline-content text)
                      (element-names)))

(define (org->ast str)
  "Parse an entire Org-mode document string @var{str} into an Abstract Syntax Tree.
This is the main entry point for the parser. It matches the @code{org-doc}
PEG pattern and then applies @code{process-ast} to resolve hierarchical
constructs like nested lists, folds `#+NAME:' and `#+CAPTION:' keywords into
the elements they describe, and resolves a link whose bare target names an
element of the document.  A single top-level element comes back as that
element's node; several come back as a list of nodes."
  (let* ((tree (parse-with org-doc str))
         (single? (and (pair? tree)
                       (symbol? (car tree))))
         (nodes (collect-named-elements (map process-ast
                                             (if single?
                                                 (list tree) tree)))))
    (if (and single?
             (pair? nodes)
             (null? (cdr nodes)))
        (car nodes) nodes)))

;; =====================================================================
;; Tree Collapsing
;; =====================================================================
;;
;; A parse comes back as a tree of fragments, and the fragments that sit next to
;; each other have to be merged into the strings they spell.  Guile's
;; `(ice-9 peg)' does that with `string-collapse', whose `string-group' builds
;; each run of adjacent strings by consing onto the run it has built so far:
;; quadratic in the length of the run.  A scanner that consumes text a character
;; at a time produces exactly such a run, one fragment per character, so for a
;; long line that quadratic term dominates the entire parse.
;;
;; The same contract is implemented here in a single pass, so collapsing is
;; linear.  This is `string-collapse' with its `string-group' rewritten, which
;; is why the trees it produces are identical rather than merely similar:
;;
;;   - anything that is not a list comes back unchanged;
;;   - every child is collapsed before its parent is looked at;
;;   - a maximal run of adjacent strings becomes their concatenation;
;;   - a list of strings is concatenated on its own and does not join the run
;;     around it, because only plain strings extend a run in `string-group';
;;   - anything else keeps its place;
;;   - a result of exactly one element is unwrapped to that element.

(define (string-list? value)
  "Return @code{#t} when @var{value} is a non-empty list of strings."
  (and (list? value)
       (pair? value)
       (every string? value)))

(define (string-group-linear lst)
  "Group runs of adjacent strings in @var{lst}, in one pass.
This is @code{(ice-9 peg simplify-tree)}'s @code{string-group} with the run
built by cons and a single reverse, so it is linear in the length of @var{lst}."
  (let loop
    ((nodes lst)
     (run '())
     (out '()))
    (cond
      ((null? nodes)
       (reverse (if (null? run) out
                    (cons (reverse run) out))))
      ((string? (car nodes))
       (loop (cdr nodes)
             (cons (car nodes) run) out))
      (else (loop (cdr nodes)
                  '()
                  (if (null? run)
                      (cons (car nodes) out)
                      (cons (car nodes)
                            (cons (reverse run) out))))))))

(define (collapse-tree tree)
  "Merge the adjacent string fragments of the parse tree @var{tree} in one pass.
Equivalent to @code{(ice-9 peg simplify-tree)}'s @code{string-collapse} for
every tree this parser produces, but linear in the number of fragments instead
of quadratic in the length of a run."
  (if (not (list? tree)) tree
      (let ((collapsed (map (lambda (child)
                              (if (string-list? child)
                                  (apply string-append child) child))
                            (string-group-linear (map collapse-tree tree)))))
        (if (and (pair? collapsed)
                 (null? (cdr collapsed)))
            (car collapsed) collapsed))))

(define (parse-with pattern str)
  "Parse @var{str} with the compiled PEG @var{pattern}, returning its tree.
This is @code{(ice-9 peg)}'s @code{match-pattern} followed by a linear tree
collapse instead of the quadratic one.  The string is copied first because a
compiled pattern caches its results by string identity, as @code{match-pattern}
itself notes."
  (let ((result (pattern (string-copy str)
                         (string-length str) 0)))
    (and result
         (collapse-tree (cadr result)))))
