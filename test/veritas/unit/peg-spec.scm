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
  (call-with-input-file path read-string))

(define sample-org-features
  (slurp "test/fixtures/sample-org-features.org"))

(define (nodes str)
  "Return the top-level AST of @var{str} as a list, even for a single element."
  (let ((ast (org->ast str)))
    (if (and (pair? ast) (symbol? (car ast))) (list ast) ast)))

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
               ("comment block" "#+BEGIN_COMMENT\nc\n#+END_COMMENT\n" comment-block)
               ("fixed width" ": code\n" fixed-width)
               ("src block" "#+BEGIN_SRC scheme\n(x)\n#+END_SRC\n" src-block)
               ("quote block" "#+BEGIN_QUOTE\nq\n#+END_QUOTE\n" quote-block)
               ("verse block" "#+BEGIN_VERSE\nv\n#+END_VERSE\n" verse-block)
               ("center block" "#+BEGIN_CENTER\nc\n#+END_CENTER\n" center-block)
               ("example block" "#+BEGIN_EXAMPLE\ne\n#+END_EXAMPLE\n" example-block)
               ("export html" "#+BEGIN_EXPORT html\n<div></div>\n#+END_EXPORT\n" export-html-block)
               ("table" "| a | b |\n|---+---|\n| 1 | 2 |\n" table)
               ("unordered list" "- a\n" unordered-list)
               ("ordered list" "1) a\n" ordered-list)
               ("description list" "- k :: v\n" description-list)
               ("separator" "-----\n" separator))
             (lambda (case)
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
               (assert-equal #:expect '(heading "**" (line-content "Title"))
                             #:got (car (nodes "** Title\n"))))
         (test "a title ending in a colon is not mistaken for tags"
               (assert-equal #:expect '(heading "*" (line-content "Note:"))
                             #:got (car (nodes "* Note:\n"))))))

(define inline-tokens-suite
  (test-each "Inline markup nodes"
             '(("*bold*" (inline-bold-content "bold"))
               ("/italic/" (inline-italic-content "italic"))
               ("~code~" (inline-code-content "code"))
               ("=verbatim=" (inline-verbatim-content "verbatim"))
               ("_underline_" (inline-underline-content "underline"))
               ("+strike+" (inline-strike-content "strike")))
             (lambda (case)
               (test (format #f "~a parses" (car case))
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
               (assert-equal #:expect '(inline-link-with-desc
                                        (link-target-content "u")
                                        (link-description-content "d"))
                             #:got (inline-org->ast "[[u][d]]"))
               (assert-equal #:expect '(inline-link-no-desc
                                        (link-target-content "u"))
                             #:got (inline-org->ast "[[u]]")))
         (test "raw urls carry their scheme"
               (assert-equal #:expect '(inline-raw-url (url-scheme "https://")
                                                       "x.org")
                             #:got (inline-org->ast "https://x.org")))
         (test "footnote reference, definition and inline form"
               (assert-equal #:expect '(inline-footnote-ref
                                        (footnote-label "1"))
                             #:got (inline-org->ast "[fn:1]"))
               (assert-equal #:expect '(inline-footnote-def
                                        (footnote-label "1")
                                        (footnote-content " note"))
                             #:got (inline-org->ast "[fn:1: note]"))
               (assert-equal #:expect '(inline-footnote-inline
                                        (footnote-content "note"))
                             #:got (inline-org->ast "[fn::note]")))))

(define whole-document-suite
  (suite "Whole-document parsing"
         (test "the feature fixture is parsed all the way to the end"
               (let ((ast (org->ast sample-org-features)))
                 (assert-length #:expect 26 #:got ast)
                 (assert-not-empty #:got ast)
                 (assert-eq #:expect 'keyword #:got (car (car ast)))))
         (test "the project's own README starts with a keyword and is long"
               (let ((ast (org->ast (slurp "README.org"))))
                 (assert-not-empty #:got ast)
                 (assert-in-range #:min 100 #:max 10000 #:got (length ast))
                 (assert-eq #:expect 'keyword #:got (car (car ast)))))))

(define (spec)
  (veritas-run document-elements-suite
               heading-metadata-suite
               inline-tokens-suite
               inline-constructs-suite
               whole-document-suite))
