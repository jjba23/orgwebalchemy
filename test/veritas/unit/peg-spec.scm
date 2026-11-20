;; orgwebalchemy
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit peg-spec)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas table)
  #:use-module (orgwebalchemy peg))

;; =======================================================================
;; Fixtures
;; =======================================================================

(define (slurp path)
  (call-with-input-file path
    read-string))

(define sample-org-features
  (slurp "test/fixtures/sample-org-features.org"))

;; The whole-document tests need these values, and `veritas' collects the
;; assertions of a test by filtering the test body for procedures: a `let'
;; around the assertions would leave only the last one visible, so the values
;; are bound here and the assertions stay at the top level of the test.
(define feature-ast
  (org->ast sample-org-features))

(define readme-ast
  (org->ast (slurp "README.org")))

(define (nodes str)
  "Return the top-level AST of @var{str} as a list, even for a single element."
  (let ((ast (org->ast str)))
    (if (and (pair? ast)
             (symbol? (car ast)))
        (list ast) ast)))

;; =======================================================================
;; Test Suites
;; =======================================================================

;; Table-driven: one case per document element.  Each case is
;; (description input expected-node-name); `test-each' expands every case into
;; its own test, so a failure names the construct that regressed.
(define document-elements-suite
  (test-each "Document elements recognise their construct"
             '(("heading" "* Title\n" heading)
               ("paragraph" "Just some text\n" paragraph)
               ("keyword" "#+TITLE: Hi\n" keyword)
               ("comment" "# a note\n" comment-line)
               ("comment block" "#+BEGIN_COMMENT\nc\n#+END_COMMENT\n"
                comment-block)
               ("fixed width" ": code\n" fixed-width)
               ("src block" "#+BEGIN_SRC scheme\n(x)\n#+END_SRC\n" src-block)
               ("quote block" "#+BEGIN_QUOTE\nq\n#+END_QUOTE\n" quote-block)
               ("verse block" "#+BEGIN_VERSE\nv\n#+END_VERSE\n" verse-block)
               ("center block" "#+BEGIN_CENTER\nc\n#+END_CENTER\n"
                center-block)
               ("example block" "#+BEGIN_EXAMPLE\ne\n#+END_EXAMPLE\n"
                example-block)
               ("export html" "#+BEGIN_EXPORT html
<div></div>
#+END_EXPORT
" export-html-block)
               ("table" "| a | b |\n|---+---|\n| 1 | 2 |\n" table)
               ("unordered list" "- a\n" org-list)
               ("ordered list" "1) a\n" org-list)
               ("description list" "- k :: v\n" org-list)
               ("mixed sibling list" "- a\n1) b\n- k :: v\n" org-list)
               ("separator" "-----\n" separator))
             (lambda (case )
               (let ((description (list-ref case 0))
                     (input (list-ref case 1))
                     (expected (list-ref case 2)))
                 (test (format #f "~a parses as ~a" description expected)
                       (assert-eq #:expect expected
                                  #:got (car (org->ast input))))))))

(define heading-metadata-suite
  (suite "Heading metadata"
         (test "TODO keyword, priority and tags are extracted"
               (assert-equal #:expect '(heading "*"
                                                (heading-todo "TODO")
                                                (heading-priority "A")
                                                (line-content "Task")
                                                (heading-tags "work" "urgent"))
                             #:got (car (nodes
                                         "* TODO [#A] Task :work:urgent:\n"))))
         (test "a TODO keyword without a priority"
               (assert-equal #:expect '(heading "*"
                                                (heading-todo "DONE")
                                                (line-content "Done"))
                             #:got (car (nodes "* DONE Done\n"))))
         (test "a plain heading keeps its simple shape"
               (assert-equal #:expect '(heading "**"
                                                (line-content "Title"))
                             #:got (car (nodes "** Title\n"))))
         (test "a title ending in a colon is not mistaken for tags"
               (assert-equal #:expect '(heading "*"
                                                (line-content "Note:"))
                             #:got (car (nodes "* Note:\n"))))))

(define inline-tokens-suite
  (test-each "Inline markup nodes"
             '(("*bold*" (inline-bold-content "bold"))
               ("/italic/" (inline-italic-content "italic"))
               ("~code~" (inline-code-content "code"))
               ("=verbatim=" (inline-verbatim-content "verbatim"))
               ("_underline_" (inline-underline-content "underline"))
               ("+strike+" (inline-strike-content "strike")))
             (lambda (case )
               (test (format #f "~a parses"
                             (car case))
                     (assert-equal #:expect (cadr case)
                                   #:got (inline-org->ast (car case)))))))

(define inline-constructs-suite
  (suite "Inline constructs"
         (test "superscript and subscript use their braced form"
               (assert-equal #:expect '((inline-text "x")
                                        (inline-superscript-content "2"))
                             #:got (inline-org->ast "x^{2}"))
               (assert-equal #:expect '((inline-text "H")
                                        (inline-subscript-content "2")
                                        (inline-text "O"))
                             #:got (inline-org->ast "H_{2}O")))
         (test "escaped markup characters are recognised"
               (assert-equal #:expect '(inline-escape-char "+")
                             #:got (inline-org->ast "\\+"))
               (assert-equal #:expect '(inline-escape-char "*")
                             #:got (inline-org->ast "\\*")))
         (test "a forced line break before end of line"
               (assert-equal #:expect '(inline-line-break "\\\\")
                             #:got (inline-org->ast "\\\\")))
         (test "links with and without a description"
               (assert-equal #:expect '(inline-link-with-desc (link-target-content
                                                               "u")
                                                              (link-description-content
                                                               "d"))
                             #:got (inline-org->ast "[[u][d]]"))
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "u"))
                             #:got (inline-org->ast "[[u]]")))
         (test "raw urls carry their scheme"
               (assert-equal #:expect '(inline-raw-url (url-scheme "https://")
                                                       "x.org")
                             #:got (inline-org->ast "https://x.org")))
         (test "footnote reference, definition and inline form"
               (assert-equal #:expect '(inline-footnote-ref (footnote-label
                                                             "1"))
                             #:got (inline-org->ast "[fn:1]"))
               (assert-equal #:expect '(inline-footnote-def (footnote-label
                                                             "1")
                                                            (footnote-content
                                                             " note"))
                             #:got (inline-org->ast "[fn:1: note]"))
               (assert-equal #:expect '(inline-footnote-inline (footnote-content
                                                                "note"))
                             #:got (inline-org->ast "[fn::note]")))))

(define whole-document-suite
  (suite "Whole-document parsing"
         (test "the feature fixture is parsed all the way to the end"
               ;; One top-level node per element, including the figure the
               ;; caption and name keywords fold into and the elements the
               ;; `#+NAME:' keywords name.
               (assert-length #:expect 40
                              #:got feature-ast)
               (assert-not-empty #:got feature-ast)
               (assert-eq #:expect 'keyword
                          #:got (car (car feature-ast))))
         (test "the project's own README starts with a keyword and is long"
               (assert-not-empty #:got readme-ast)
               (assert-in-range #:min 100
                                #:max 10000
                                #:got (length readme-ast))
               (assert-eq #:expect 'keyword
                          #:got (car (car readme-ast))))))

(define mixed-list-suite
  (suite "Mixed and nested lists"
         (test "a list keeps every kind of item in one run"
               (assert-equal #:expect '(org-list (unordered-item (line-content
                                                                  "u"))
                                                 (ordered-item (line-content
                                                                "o"))
                                                 (desc-item (desc-key "k")
                                                            (line-content "v")))
                             #:got (car (nodes "- u\n1) o\n- k :: v\n"))))
         (test "an ordered list nests under an unordered item"
               (assert-equal #:expect '(org-list (unordered-item (line-content
                                                                  "outer")
                                                                 (nested-list (ordered-item
                                                                               (line-content
                                                                                "n1"))

                                                                              
                                                                              (ordered-item
                                                                               (line-content
                                                                                "n2")))))
                             #:got (car (nodes "- outer\n  1. n1\n  2. n2\n"))))
         (test "a nested list may mix item kinds"
               (assert-equal #:expect '(org-list (unordered-item (line-content
                                                                  "a")
                                                                 (nested-list (unordered-item
                                                                               (line-content
                                                                                "b"))

                                                                              
                                                                              (ordered-item
                                                                               (line-content
                                                                                "c"))

                                                                              
                                                                              (unordered-item
                                                                               (line-content
                                                                                "d")))))
                             #:got (car (nodes "- a\n  - b\n  1. c\n  - d\n"))))
         (test "a description list nests under an unordered item"
               (assert-equal #:expect '(org-list (unordered-item (line-content
                                                                  "term")
                                                                 (nested-list (desc-item
                                                                               (desc-key
                                                                                "k")

                                                                               
                                                                               (line-content
                                                                                "v")))))
                             #:got (car (nodes "- term\n  - k :: v\n"))))
         (test "two levels of mixed nesting"
               (assert-equal #:expect '(org-list (unordered-item (line-content
                                                                  "a")
                                                                 (nested-list (unordered-item
                                                                               (line-content
                                                                                "b")

                                                                               
                                                                               (nested-list
                                                                                (ordered-item
                                                                                 (line-content
                                                                                  "c"))))

                                                                              
                                                                              (unordered-item
                                                                               (line-content
                                                                                "d")))))
                             #:got (car (nodes "- a\n  - b\n    1. c\n  - d\n"))))))

;; Link targets are classified in one place so that both renderers agree on
;; what a target means.  These tests pin that classification down.
(define link-targets-suite
  (suite "Org link targets"
         (test "a scheme link keeps its scheme"
               (assert-equal #:expect 'https
                             #:got (org-link-protocol "https://example.org/a"))
               (assert-equal #:expect "//example.org/a"
                             #:got (org-link-path "https://example.org/a"))
               (assert-equal #:expect "https://example.org/a"
                             #:got (org-link-href "https://example.org/a")))
         (test "mailto and tel links keep their scheme"
               (assert-equal #:expect 'mailto
                             #:got (org-link-protocol "mailto:me@example.org"))
               (assert-equal #:expect "mailto:me@example.org"
                             #:got (org-link-href "mailto:me@example.org"))
               (assert-equal #:expect 'tel
                             #:got (org-link-protocol "tel:+1234")))
         (test "file links and bare paths are local files"
               (assert-equal #:expect 'file
                             #:got (org-link-protocol "file:notes.org"))
               (assert-equal #:expect "notes.org"
                             #:got (org-link-path "file:notes.org"))
               (assert-equal #:expect "notes.org"
                             #:got (org-link-href "file:notes.org"))
               (assert-equal #:expect 'file
                             #:got (org-link-protocol "notes.org"))
               (assert-equal #:expect "images/a.png"
                             #:got (org-link-path "file://images/a.png")))
         (test "a ::search suffix is kept apart from the path"
               (assert-equal #:expect "notes.org"
                             #:got (org-link-path "notes.org::*Intro"))
               (assert-equal #:expect "*Intro"
                             #:got (org-link-search "notes.org::*Intro"))
               (assert-equal #:expect "notes.org"
                             #:got (org-link-href "notes.org::*Intro"))
               (assert-equal #:expect #f
                             #:got (org-link-search "notes.org")))
         (test "custom ids and heading links become fragments"
               (assert-equal #:expect 'internal
                             #:got (org-link-protocol "#custom-id"))
               (assert-equal #:expect "#custom-id"
                             #:got (org-link-href "#custom-id"))
               (assert-equal #:expect "custom-id"
                             #:got (org-link-display "#custom-id"))
               (assert-equal #:expect 'heading
                             #:got (org-link-protocol "*My Section"))
               (assert-equal #:expect "#my-section"
                             #:got (org-link-href "*My Section"))
               (assert-equal #:expect "My Section"
                             #:got (org-link-display "*My Section")))
         (test "a Windows drive letter is not a scheme"
               (assert-equal #:expect 'file
                             #:got (org-link-protocol "C:/tmp/x.txt"))
               (assert-equal #:expect "C:/tmp/x.txt"
                             #:got (org-link-href "C:/tmp/x.txt")))
         (test "image links are recognised by extension"
               (assert-equal #:expect #t
                             #:got (org-image-link? "file:photo.png"))
               (assert-equal #:expect #t
                             #:got (org-image-link? "images/diagram.SVG"))
               (assert-equal #:expect #t
                             #:got (org-image-link? "photo.jpeg?raw=1"))
               (assert-equal #:expect #t
                             #:got (org-image-link?
                                    "data:image/gif;base64,AAAA"))
               (assert-equal #:expect #f
                             #:got (org-image-link? "file:notes.org"))
               (assert-equal #:expect #f
                             #:got (org-image-link?
                                    "https://example.org/x.png")))))

;; A scheme the parser does not know is classified and then left as written; a
;; host application says what it means by binding `link-protocols'.  No scheme
;; but the ones Org itself defines appears in the parser, so the registry below
;; is the host's and the parser stays a parser.
(define you-tube-href
  (lambda (path)
    (format #f "https://www.youtube.com/watch?v=~a" path)))

(define custom-protocols-suite
  (suite "Custom link protocols"
         (test "an unknown scheme is classified and kept as written"
               (assert-equal #:expect 'yt
                             #:got (org-link-protocol "yt:dQw4w9WgXcQ"))
               (assert-equal #:expect "dQw4w9WgXcQ"
                             #:got (org-link-path "yt:dQw4w9WgXcQ"))
               (assert-equal #:expect "yt:dQw4w9WgXcQ"
                             #:got (org-link-href "yt:dQw4w9WgXcQ")))
         (test "a registered protocol resolves to the href its resolver gives"
               (assert-equal #:expect "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
                             #:got (parameterize ((link-protocols (list (cons 'yt
                                                                            you-tube-href))))
                                     (org-link-href "yt:dQw4w9WgXcQ"))))
         (test "a resolver that declines leaves the target as written"
               ;; `#f' is how a resolver says it does not know this one, so a
               ;; registry entry never has to guess a fallback.
               (assert-equal #:expect "zotero:ITEMKEY"
                             #:got (parameterize ((link-protocols (list (cons 'zotero
                                                                             (lambda (path)
                                                                               #f)))))
                                     (org-link-href "zotero:ITEMKEY"))))
         (test "a resolver is handed the path, without the ::search suffix"
               (assert-equal #:expect '("dQw4w9WgXcQ")
                             #:got (parameterize ((link-protocols (list (cons 'yt
                                                                            (lambda (path)
                                                                              path)))))
                                     (list (org-link-href "yt:dQw4w9WgXcQ::*Chorus")))))
         (test "a link without a description shows the target, not the resolution"
               ;; The document says `yt:...'; the href is the host's business,
               ;; and a renderer that showed it would be answering a question
               ;; nobody asked.
               (assert-equal #:expect "yt:dQw4w9WgXcQ"
                             #:got (parameterize ((link-protocols (list (cons 'yt
                                                                            you-tube-href))))
                                     (org-link-display "yt:dQw4w9WgXcQ"))))
         (test "a resolver wins over the href the parser would have given"
               (assert-equal #:expect "/assets/notes.org"
                             #:got (parameterize ((link-protocols (list (cons 'file
                                                                            (lambda (path)
                                                                              (string-append "/assets/"
                                                                                             path))))))
                                     (org-link-href "file:notes.org")))
               (assert-equal #:expect "notes.org"
                             #:got (org-link-display "file:notes.org")))
         (test "no registry leaves the parser's own classification alone"
               (assert-equal #:expect 'file
                             #:got (org-link-protocol "notes.org"))
               (assert-equal #:expect "#custom-id"
                             #:got (org-link-href "#custom-id"))
               (assert-equal #:expect "https://example.org/a"
                             #:got (org-link-href "https://example.org/a")))))

;; Headings carry their anchor as an id, and `*Heading' links point at the very
;; same anchor, so the two can never drift apart.
(define heading-anchors-suite
  (suite "Heading anchors"
         (test "titles become URL fragments"
               (assert-equal #:expect "my-first-section"
                             #:got (org-anchor "My First Section"))
               (assert-equal #:expect "spaced-punct"
                             #:got (org-anchor "  Spaced & Punct!!  "))
               (assert-equal #:expect "a-b"
                             #:got (org-anchor "a -- b"))
               (assert-equal #:expect ""
                             #:got (org-anchor "***")))
         (test "a heading link uses the anchor of its title"
               (assert-equal #:expect (string-append "#"
                                                     (org-anchor "My Section"))
                             #:got (org-link-href "*My Section")))))

;; A `#+CAPTION:' / `#+NAME:' keyword run describing an image link is folded
;; into one `figure' node, so the renderers are handed a single element and the
;; name the figure carries is the target a `[[fig:arch]]' link resolves to.
(define figures-suite
  (suite "Figures"
         (test "a name, a caption and an image become one figure"
               (assert-equal #:expect '(figure (figure-name "fig:arch")
                                               (figure-caption
                                                "The architecture")
                                               (figure-image (figure-target
                                                              "file:diagram.png")
                                                             (figure-description
                                                              "a diagram")))
                             #:got (car (nodes (string-append
                                                "#+NAME: fig:arch\n"
                                                "#+CAPTION: The architecture\n"
                                                "[[file:diagram.png][a diagram]]\n")))))
         (test "a name alone is still a figure"
               (assert-equal #:expect '(figure (figure-name "fig:x")
                                               (figure-image (figure-target
                                                              "images/x.svg")))
                             #:got (car (nodes
                                               "#+NAME: fig:x\n[[images/x.svg]]\n"))))
         (test "a caption alone is still a figure"
               (assert-equal #:expect '(figure (figure-caption "A caption")
                                               (figure-image (figure-target
                                                              "file:a.png")
                                                             (figure-description
                                                              "alt")))
                             #:got (car (nodes (string-append
                                                "#+CAPTION: A caption\n"
                                                "[[file:a.png][alt]]\n")))))
         (test "several captions join in document order"
               (assert-equal #:expect '(figure (figure-caption "One two")
                                               (figure-image (figure-target
                                                              "file:a.png")))
                             #:got (car (nodes (string-append
                                                "#+CAPTION: One\n"
                                                "#+CAPTION: two\n"
                                                "[[file:a.png]]\n")))))
         (test "a caption describing something else stays a keyword"
               (assert-equal #:expect '(keyword (keyword-key "CAPTION")
                                                (keyword-value "A table"))
                             #:got (car (nodes "#+CAPTION: A table\n| a |\n")))
               (assert-equal #:expect '(keyword (keyword-key "CAPTION")
                                                (keyword-value "Not a figure"))
                             #:got (car (nodes (string-append
                                                "#+CAPTION: Not a figure\n"
                                                "[[https://example.org][link]]\n")))))
         (test "a link to an image elsewhere is no local figure"
               (assert-equal #:expect '(keyword (keyword-key "CAPTION")
                                                (keyword-value "c"))
                             #:got (car (nodes (string-append "#+CAPTION: c\n"
                                                "[[https://example.org/a.png]]\n")))))
         (test "an empty name is no name"
               ;; The value of an empty `#+NAME:' is the empty string, not the
               ;; bare tag symbol an empty match used to leave behind; either
               ;; way there is no name here, so the image stays an image.
               (assert-equal #:expect '(keyword (keyword-key "NAME")
                                                (keyword-value ""))
                             #:got (car (nodes "#+NAME:\n[[file:a.png]]\n"))))
         (test "a figure keeps its place among the paragraphs around it"
               (assert-equal #:expect '((paragraph (line-content "before"))
                                        (figure (figure-caption "c")
                                                (figure-image (figure-target
                                                               "file:a.png")))
                                        (paragraph (line-content "after")))
                             #:got (nodes (string-append "before\n"
                                                         "#+CAPTION: c\n"
                                                         "[[file:a.png]]\n"
                                                         "after\n"))))
         (test "a figure reports its parts, missing ones as #f"
               (assert-equal #:expect '("fig:arch" "The architecture"
                                        "file:diagram.png" "a diagram")
                             #:got (figure-parts (car (nodes (string-append
                                                              "#+NAME: fig:arch\n"
                                                              "#+CAPTION: The architecture\n"
                                                              "[[file:diagram.png][a diagram]]\n")))))
               (assert-equal #:expect '(#f "A caption" "file:a.png" #f)
                             #:got (figure-parts (car (nodes (string-append
                                                              "#+CAPTION: A caption\n"
                                                              "[[file:a.png]]\n"))))))))

;; A `#+NAME:' targets an element, the way it targets a figure, so the id a
;; cross-reference link points at is the id the element carries and neither
;; `[[tbl:data]]' nor a bare `[[name]]' can point at nothing.
(define named-elements-suite
  (suite "Named elements"
         (test "a name becomes the id of the element under it"
               (assert-equal #:expect '(named (element-name "tbl:data")
                                              (table (table-row (table-cell
                                                                 " a "))))
                             #:got (car (nodes (string-append
                                                "#+NAME: tbl:data\n" "| a |\n"))))
               (assert-equal #:expect '(named (element-name "q")
                                              (quote-block (quote-content
                                                                          "x\n")))
                             #:got (car (nodes (string-append "#+NAME: q\n"
                                                              "#+BEGIN_QUOTE\nx\n"
                                                              "#+END_QUOTE\n")))))
         (test "a named element reports its name and itself"
               (assert-equal #:expect '("code" (src-block (lang "scheme")
                                                          (src-content "(x)\n")))
                             #:got (named-parts (car (nodes (string-append
                                                             "#+NAME: code\n"
                                                             "#+BEGIN_SRC scheme\n"
                                                             "(x)\n"
                                                             "#+END_SRC\n"))))))
         (test "a name in front of nothing stays a keyword"
               (assert-equal #:expect '(keyword (keyword-key "NAME")
                                                (keyword-value "lonely"))
                             #:got (org->ast "#+NAME: lonely\n"))
               (assert-equal #:expect '((keyword (keyword-key "NAME")
                                                 (keyword-value "c"))
                                        (comment-line (comment-content "x")))
                             #:got (nodes "#+NAME: c\n# x\n")))
         (test "a caption before a table keeps its place, in document order"
               (assert-equal #:expect '((keyword (keyword-key "CAPTION")
                                                 (keyword-value "c"))
                                        (named (element-name "n")
                                               (paragraph (line-content "t"))))
                             #:got (nodes (string-append "#+CAPTION: c\n"
                                                         "#+NAME: n\n" "t\n"))))
         (test "a named element still names the image below it a figure"
               (assert-equal #:expect '(figure (figure-name "diagram")
                                               (figure-image (figure-target
                                                              "file:d.png")))
                             #:got (car (nodes (string-append
                                                "#+NAME: diagram\n"
                                                "[[file:d.png]]\n")))))
         (test "the names of a document are the names its elements carry"
               (assert-equal #:expect '("tbl:data" "diagram")
                             #:got (document-names (org->ast (string-append
                                                              "#+NAME: tbl:data\n"
                                                              "| a |\n\n"
                                                              "#+NAME: diagram\n"
                                                              "[[file:d.png]]\n"))))
               (assert-equal #:expect '()
                             #:got (document-names (org->ast "no names here\n"))))
         (test "a bare target naming an element becomes a fragment link"
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "#diagram"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[diagram]]")))
               (assert-equal #:expect '(inline-link-with-desc (link-target-content
                                                               "#diagram")
                                                              (link-description-content
                                                               "it"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[diagram][it]]"))))
         (test "a prefixed cross-reference needs no rewriting"
               (assert-equal #:expect "#tbl:data"
                             #:got (org-link-href "tbl:data"))
               (assert-equal #:expect "#fig:arch"
                             #:got (org-link-href "fig:arch")))
         (test "a name wins over the file path it would otherwise be"
               ;; A bare target that is exactly a name describes the element,
               ;; so it becomes a fragment even when it looks like a file name.
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "#notes.org"))
                             #:got (parameterize ((element-names '("notes.org")))
                                     (inline-org->ast "[[notes.org]]"))))
         (test "a target that says what it is is left alone"
               ;; A URL, a `#' fragment and a `*Heading' all keep their
               ;; meaning, and so does a target no element of the document
               ;; carries a name for.
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "https://x/diagram"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[https://x/diagram]]")))
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "#diagram"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[#diagram]]")))
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "*diagram"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[*diagram]]")))
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "diagram::*H"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[diagram::*H]]")))
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "nowhere"))
                             #:got (parameterize ((element-names '("diagram")))
                                     (inline-org->ast "[[nowhere]]"))))
         (test "no names in scope leaves every target as written"
               (assert-equal #:expect '(inline-link-no-desc (link-target-content
                                                             "diagram"))
                             #:got (inline-org->ast "[[diagram]]")))))

;; The line, cell and inline scanners hand the tree one fragment per run of
;; text rather than one per character.  These tests pin down where a run is
;; allowed to end, which is the part that must not drift.
(define scanners-suite
  (suite "Run scanners"
         (test "a long line is one string, not one fragment per character"
               (let ((content (cadr (car (nodes (string-append (make-string
                                                                            10000
                                                                            #\z)
                                                               "\n"))))))
                 (assert-equal #:expect 'line-content
                               #:got (car content))
                 (assert-true (string? (cadr content)))
                 (assert-equal #:expect 10000
                               #:got (string-length (cadr content)))))
         (test "a plain inline run is one string, not one per character"
               (assert-equal #:expect '(inline-text "plain text here")
                             #:got (inline-org->ast "plain text here"))
               (assert-equal #:expect '(inline-text
                                        "the h f s w g t m i r letters")
                             #:got (inline-org->ast
                                    "the h f s w g t m i r letters")))
         (test "a run ends where an inline construct really begins"
               (assert-equal #:expect '((inline-text "see")
                                        (inline-raw-url (url-scheme "https://")
                                                        "example.org/a"))
                             #:got (inline-org->ast "seehttps://example.org/a"))
               (assert-equal #:expect '((inline-text "a")
                                        (inline-bold-content "b"))
                             #:got (inline-org->ast "a*b*")))
         (test "a tab and non-ASCII text are ordinary line content"
               (assert-equal #:expect '(paragraph (line-content "a\tb"))
                             #:got (car (nodes "a\tb\n")))
               (assert-equal #:expect '(paragraph (line-content "café 中文"))
                             #:got (car (nodes "café 中文\n"))))
         (test "a line ends at a newline, and a table cell at its separator"
               (assert-equal #:expect '(table (table-row (table-cell " a ")
                                                         (table-cell " b ")))
                             #:got (car (nodes "| a | b |\n")))
               (assert-equal #:expect '(table (table-row (table-cell "   ")))
                             #:got (car (nodes "|   |\n"))))
         (test "a lone carriage return is content, as it always was"
               (assert-equal #:expect '(table (table-row (table-cell " a\rb ")))
                             #:got (car (nodes "| a\rb |\n")))
               (assert-equal #:expect '(paragraph (line-content "a"))
                             #:got (car (nodes "a\nb\n"))))
         (test
          "a table row needs no trailing newline, and gains no extra cell"
          ;; The closing bar of a row that ends the input used to be read as
          ;; though one more empty cell followed it, so `| a |' was a row of
          ;; two cells while `| a |\n' was a row of one.
          (assert-equal #:expect '(table (table-row (table-cell " a ")))
                        #:got (car (nodes "| a |")))
          (assert-equal #:expect '(table (table-row (table-cell " a ")
                                                    (table-cell " b ")))
                        #:got (car (nodes "| a | b |"))))
         (test
          "a table line that is only its closing bar holds one empty cell"
          ;; Org reads `|' as a row of one empty field.  The bar a cell is
          ;; written after is the one the test for a row end cannot see,
          ;; because a row has to open with a bar.
          (assert-equal #:expect '(table (table-row (table-cell "")))
                        #:got (car (nodes "|\n")))
          (assert-equal #:expect '(table (table-row (table-cell "")))
                        #:got (car (nodes "||\n")))
          (assert-equal #:expect '(table (table-row (table-cell " a ")
                                                    (table-cell "")))
                        #:got (car (nodes "| a ||\n"))))
         (test "an empty value is text, not a gap in the tree"
               ;; A pattern that accumulates `all' turns an empty match into a
               ;; bare tag symbol, which is how an empty cell used to lose its
               ;; column and a block written without a language used to vanish.
               (assert-equal #:expect '(table (table-row (table-cell "a")
                                                         (table-cell "")
                                                         (table-cell "b")))
                             #:got (car (nodes "|a||b|\n")))
               (assert-equal #:expect '(src-block (lang "")
                                                  (src-content "(x)\n"))
                             #:got (car (nodes "#+BEGIN_SRC\n(x)\n#+END_SRC\n")))
               (assert-equal #:expect '(keyword (keyword-key "KEY")
                                                (keyword-value ""))
                             #:got (car (nodes "#+KEY:\n")))
               (assert-equal #:expect '(quote-block (quote-content ""))
                             #:got (car (nodes "#+BEGIN_QUOTE\n#+END_QUOTE\n"))))
         (test "the last line of a document without a newline parses"
               (assert-equal #:expect '(paragraph (line-content "text"))
                             #:got (org->ast "text")))))

(define (spec)
  (veritas-run document-elements-suite
               heading-metadata-suite
               inline-tokens-suite
               inline-constructs-suite
               mixed-list-suite
               link-targets-suite
               custom-protocols-suite
               heading-anchors-suite
               figures-suite
               named-elements-suite
               scanners-suite
               whole-document-suite))
