;; orgwebalchemy
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit json-spec)
  #:use-module (ice-9 rdelim)
  #:use-module (veritas veritas)
  #:use-module (veritas snapshot)
  #:use-module (orgwebalchemy peg)
  #:use-module (orgwebalchemy json))

(define (slurp path)
  (call-with-input-file path
    read-string))

(define sample-org-features
  (slurp "test/fixtures/sample-org-features.org"))

;; The JSON tree is a contract, so these tests spell the expected documents out
;; in full rather than reaching into the tree: a node is `{"type": ..., "children":
;; [...]}', a document is always an array, and prose is always the inline nodes
;; the renderers see, never the string the parser kept.

(define json-strings-suite
  (suite "Strings as JSON"
         (test "a plain string is one JSON string"
               (assert-equal #:expect "\"text\""
                             #:got (json-string "text")))
         (test "a quote and a backslash are escaped"
               (assert-equal #:expect "\"say \\\"hi\\\"\""
                             #:got (json-string "say \"hi\""))
               (assert-equal #:expect "\"a\\\\b\""
                             #:got (json-string "a\\b")))
         (test "the whitespace JSON names is named, not numbered"
               (assert-equal #:expect "\"a\\tb\""
                             #:got (json-string "a\tb"))
               (assert-equal #:expect "\"a\\nb\""
                             #:got (json-string "a\nb"))
               (assert-equal #:expect "\"a\\rb\""
                             #:got (json-string "a\rb")))
         (test "any other control character becomes a \\u escape"
               (assert-equal #:expect "\"\\u0001\""
                             #:got (json-string "\x01"))
               (assert-equal #:expect "\"\\u001f\""
                             #:got (json-string "\x1f")))
         (test "text outside ASCII is written as itself"
               (assert-equal #:expect "\"café 中文\""
                             #:got (json-string "café 中文")))))

(define document-json-suite
  (suite "Document to JSON"
         (test "one element is a one-item array"
               (assert-equal #:expect
                             "[{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"text\"]}]}]}]"
                             #:got (org->json "text")))
         (test "an empty document is an empty array"
               (assert-equal #:expect "[]"
                             #:got (org->json "")))
         (test "the elements keep document order"
               (assert-equal #:expect
                             "[{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"one\"]}]}]},{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"two\"]}]}]}]"
                             #:got (org->json "one\n\ntwo\n")))
         (test "a heading carries its stars and its metadata as children"
               (assert-equal #:expect
                             "[{\"type\":\"heading\",\"children\":[\"*\",{\"type\":\"heading-todo\",\"children\":[\"TODO\"]},{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"Heading\"]}]},{\"type\":\"heading-tags\",\"children\":[\"tag\"]}]}]"
                             #:got (org->json "* TODO Heading :tag:\n")))
         (test "a node with no children is still a node"
               ;; An empty match collapses to the bare tag symbol in the tree,
               ;; and a table's separator row is one: it is a node, not a gap.
               (assert-equal #:expect
                             "[{\"type\":\"table\",\"children\":[{\"type\":\"table-header-row\",\"children\":[{\"type\":\"table-cell\",\"children\":[{\"type\":\"inline-text\",\"children\":[\" a \"]}]}]},{\"type\":\"table-separator-row\",\"children\":[]},{\"type\":\"table-row\",\"children\":[{\"type\":\"table-cell\",\"children\":[{\"type\":\"inline-text\",\"children\":[\" c \"]}]}]}]}]"
                             #:got (org->json "| a |\n|---|\n| c |\n")))
         (test "a block keeps its literal content as one string"
               (assert-equal #:expect
                             "[{\"type\":\"src-block\",\"children\":[{\"type\":\"lang\",\"children\":[\"scheme\"]},{\"type\":\"src-content\",\"children\":[\"(x)\\n\"]}]}]"
                             #:got (org->json
                                    "#+BEGIN_SRC scheme\n(x)\n#+END_SRC\n")))))

(define prose-json-suite
  (suite "Prose is resolved before it is serialised"
         (test "inline markup becomes nodes"
               (assert-equal #:expect
                             "[{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-bold-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"bold\"]}]},{\"type\":\"inline-text\",\"children\":[\" and \"]},{\"type\":\"inline-code-content\",\"children\":[\"code\"]}]}]}]"
                             #:got (org->json "*bold* and ~code~\n")))
         (test "a table cell is prose"
               (assert-equal #:expect
                             "[{\"type\":\"table\",\"children\":[{\"type\":\"table-row\",\"children\":[{\"type\":\"table-cell\",\"children\":[{\"type\":\"inline-text\",\"children\":[\" a \"]}]},{\"type\":\"table-cell\",\"children\":[{\"type\":\"inline-text\",\"children\":[\" \"]},{\"type\":\"inline-bold-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"b\"]}]},{\"type\":\"inline-text\",\"children\":[\" \"]}]}]}]}]"
                             #:got (org->json "| a | *b* |\n")))
         (test "a description term and its detail are prose"
               (assert-equal #:expect
                             "[{\"type\":\"org-list\",\"children\":[{\"type\":\"desc-item\",\"children\":[{\"type\":\"desc-key\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"Term\"]}]},{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"Definition\"]}]}]}]}]"
                             #:got (org->json "- Term :: Definition\n")))
         (test "a link description is resolved, not left as a string"
               (assert-equal #:expect
                             "[{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-link-with-desc\",\"children\":[{\"type\":\"link-target-content\",\"children\":[\"https://x\"]},{\"type\":\"link-description-content\",\"children\":[{\"type\":\"inline-bold-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"hi\"]}]}]}]}]}]}]"
                             #:got (org->json "[[https://x][*hi*]]\n")))
         (test "a bare target naming an element is the fragment the renderers use"
               (assert-contains #:expect (org->json (string-append
                                                     "#+NAME: diagram\n"
                                                     "[[file:d.png]]\n\n"
                                                     "See [[diagram]].\n"))
                                #:got "\"#diagram\""))
         (test "a custom protocol target is serialised as the document wrote it"
               ;; The registry decides an href at render time; the tree keeps
               ;; the target, so a JSON snapshot cannot depend on it.
               (assert-contains #:expect (parameterize ((link-protocols
                                                         (list (cons 'yt
                                                                     (lambda (path)
                                                                       "https://example.org")))))
                                          (org->json "[[yt:abc]]\n"))
                                #:got "\"yt:abc\""))
         (test "an inline string serialises on its own"
               (assert-equal #:expect
                             "[{\"type\":\"inline-text\",\"children\":[\"a \"]},{\"type\":\"inline-bold-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"b\"]}]}]"
                             #:got (inline-org->json "a *b*")))))

(define fixture-json-suite
  (suite "The fixture as JSON"
         (test "the whole feature fixture matches its snapshot"
               #:tags '(document)
               (assert-snapshot #:name "sample-org-features json"
                                #:got (lambda ()
                                        (org->json sample-org-features))))
         (test "an AST built by hand serialises the same way"
               ;; `org-ast->json' is a function of a tree, so a host that built
               ;; or edited one gets the same document `org->json' gives.
               (assert-equal #:expect
                             "[{\"type\":\"paragraph\",\"children\":[{\"type\":\"line-content\",\"children\":[{\"type\":\"inline-text\",\"children\":[\"x\"]}]}]}]"
                             #:got (org-ast->json '(paragraph (line-content
                                                               "x")))))))

(define (spec)
  (veritas-run json-strings-suite
               document-json-suite
               prose-json-suite
               fixture-json-suite))
