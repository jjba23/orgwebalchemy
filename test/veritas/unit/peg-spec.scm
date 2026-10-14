;; orgwebalchemy

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
               (assert-length #:expect 33
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

(define (spec)
  (veritas-run document-elements-suite
               heading-metadata-suite
               inline-tokens-suite
               inline-constructs-suite
               mixed-list-suite
               link-targets-suite
               heading-anchors-suite
               whole-document-suite))
