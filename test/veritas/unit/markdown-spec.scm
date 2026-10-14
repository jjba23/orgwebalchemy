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

(define-module (veritas unit markdown-spec)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (veritas table)
  #:use-module (veritas snapshot)
  #:use-module (orgwebalchemy peg)
  #:use-module (orgwebalchemy markdown))

;; =======================================================================
;; Fixtures
;; =======================================================================

(define (slurp path)
  (call-with-input-file path
    read-string))

(define sample-org-features
  (slurp "test/fixtures/sample-org-features.org"))

(define sample-org-text
  (slurp "test/fixtures/sample-org-text.org"))

;; =======================================================================
;; Helpers
;; =======================================================================

(define (render-inline text)
  (inline-org->markdown (inline-org->ast text)))

;; =======================================================================
;; Test Suites
;; =======================================================================

(define inline-markdown-suite
  (test-each "Inline Org renders to Markdown"
             '(("*bold*" "**bold**")
               ("/italic/" "*italic*")
               ("~code~" "`code`")
               ("=verbatim=" "`verbatim`")
               ("_underline_" "<u>underline</u>")
               ("+strike+" "~~strike~~")
               ("x^{2}" "x<sup>2</sup>")
               ("H_{2}O" "H<sub>2</sub>O")
               ("[[u][d]]" "[d](u)")
               ("[[u]]" "[u](u)")
               ("https://x.org" "<https://x.org>")
               ("[fn:1]" "[^1]")
               ("[fn::note]" "^[note]")
               ("plain" "plain"))
             (lambda (case )
               (test (format #f "~a -> ~a"
                             (car case)
                             (cadr case))
                     (assert-equal #:expect (cadr case)
                                   #:got (render-inline (car case)))))))

(define block-markdown-suite
  (test-each "Block Org renders to Markdown"
             '(("heading" "* Title\n" "# Title\n\n")
               ("heading metadata" "* TODO [#A] T :x:\n"
                "# TODO [#A] T :x:\n\n")
               ("paragraph" "Some text\n" "Some text\n\n")
               ("separator" "-----\n" "---\n\n")
               ("unordered list" "- a\n- b\n" "- a\n- b\n\n")
               ("ordered list" "1) a\n2) b\n" "1. a\n2. b\n\n")
               ("checkbox off" "- [ ] a\n" "- [ ] a\n\n")
               ("checkbox on" "- [X] a\n" "- [x] a\n\n")
               ("description list" "- k :: v\n" "**k**\n:   v\n\n")
               ("src block" "#+BEGIN_SRC scheme\n(x)\n#+END_SRC\n"
                "```scheme\n(x)\n```\n\n")
               ("example block" "#+BEGIN_EXAMPLE\nx\n#+END_EXAMPLE\n"
                "```\nx\n```\n\n")
               ("quote block" "#+BEGIN_QUOTE\nhi\n#+END_QUOTE\n" "> hi\n\n")
               ("keyword" "#+TITLE: Hi\n" "TITLE: Hi\n\n")
               ("comment" "# a comment\n" "<!-- a comment -->\n\n")
               ("fixed width" ": fixed\n" "    fixed\n\n")
               ("verse block" "#+BEGIN_VERSE\na\nb\n#+END_VERSE\n"
                "a  \nb\n\n")
               ("table" "| Name | Age |
|------+-----|
| Ada | 36 |
" "| Name | Age |
| --- | --- |
| Ada | 36 |

"))
             (lambda (case )
               (test (format #f "~a renders"
                             (car case))
                     (assert-equal #:expect (list-ref case 2)
                                   #:got (org->markdown (list-ref case 1)))))))

(define nested-markdown-suite
  (suite "Nested Markdown structures"
         (test "nested unordered lists are indented"
               (assert-equal #:expect "- a\n  - b\n\n"
                             #:got (org->markdown "- a\n  - b\n")))
         (test "nested ordered lists are indented"
               (assert-equal #:expect "1. a\n   1. b\n\n"
                             #:got (org->markdown "1) a\n   1. b\n")))
         (test "a checkbox nested inside a list"
               (assert-equal #:expect "- a\n  - [ ] b\n\n"
                             #:got (org->markdown "- a\n  - [ ] b\n")))))

;; `suite-each' expands one case into a whole suite of its own, which is handy
;; when every fixture deserves more than a single check.
(define fixtures->markdown-suite
  (suite-each "Every fixture converts to Markdown"
              '("sample-org-features.org" "sample-org-text.org"
                "sample-org-blocks.org" "sample-org-table.org")
              (lambda (fixture)
                (let ((source (slurp (format #f "test/fixtures/~a" fixture))))
                  (suite (format #f "~a" fixture)
                         (test "converts without error"
                               (assert-no-error (lambda ()
                                                  (org->markdown source))
                                                #:timeout 5))
                         (test "produces a non-empty document"
                               (assert-not-empty #:got (org->markdown source))))))))

(define mixed-list-suite
  (suite "Mixed and nested list types"
         (test "consecutive kinds become separate Markdown lists"
               (assert-equal #:expect "- u\n1. o\n**k**\n:   v\n\n"
                             #:got (org->markdown "- u\n1) o\n- k :: v\n")))
         (test "an ordered list nests and keeps its indentation"
               (assert-equal #:expect "- outer\n  1. n1\n  2. n2\n\n"
                             #:got (org->markdown
                                                  "- outer\n  1. n1\n  2. n2\n")))
         (test "a mixed nested list renders each run"
               (assert-equal #:expect "- a\n  - b\n  1. c\n  - d\n\n"
                             #:got (org->markdown
                                                  "- a\n  - b\n  1. c\n  - d\n")))
         (test "a description list nests under an unordered item"
               (assert-equal #:expect "- term\n  **k**\n  :   v\n\n"
                             #:got (org->markdown "- term\n  - k :: v\n")))))

(define document-markdown-suite
  (suite "Full document Markdown"
         (test "the feature fixture matches its snapshot"
               #:tags '(document)
               (assert-snapshot #:name "sample-org-features markdown"
                                #:got (lambda ()
                                        (org->markdown sample-org-features))))
         (test "the fixture keeps the metadata keyword"
               (assert-contains #:expect (org->markdown sample-org-features)
                                #:got "title: Feature Showcase"))
         (test "the fixture keeps inline markup"
               (assert-contains #:expect (org->markdown sample-org-features)
                                #:got "**bold**")
               (assert-contains #:expect (org->markdown sample-org-features)
                                #:got "~~strikethrough~~"))
         (test "the sample text fixture converts without error"
               (assert-no-error (lambda ()
                                  (org->markdown sample-org-text)))
               (assert-matches #:pattern "# Top Level Heading"
                               #:got (org->markdown sample-org-text)))))

(define (spec)
  (veritas-run inline-markdown-suite
               block-markdown-suite
               nested-markdown-suite
               mixed-list-suite
               fixtures->markdown-suite
               document-markdown-suite))
