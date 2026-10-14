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

(define-module (veritas unit html-spec)
  #:use-module (ice-9 rdelim)
  #:use-module (veritas veritas)
  #:use-module (orgwebalchemy peg)
  #:use-module (orgwebalchemy html))

;; =======================================================================
;; Fixtures
;; =======================================================================

(define (fixture-to-string name)
  (let ((fixture-path (format #f "test/fixtures/~a" name)))
    (call-with-input-file fixture-path
      read-string)))

(define sample-org-text
  (fixture-to-string "sample-org-text.org"))

(define sample-org-blocks
  (fixture-to-string "sample-org-blocks.org"))

(define sample-org-table
  (fixture-to-string "sample-org-table.org"))

(define sample-org-features
  (fixture-to-string "sample-org-features.org"))

;; =======================================================================
;; Test Suites
;; =======================================================================

(define make-sxml-attrs-suite
  (suite "make-sxml-attrs"
         (test
          "constructs a class attribute when classes and attributes are supplied"
          (assert-equal #:got (make-sxml-attrs '("heading")
                                               '((href "/about")))
                        #:expect '((@ (class "heading")
                                      (href "/about"))))
          (assert-equal #:got (make-sxml-attrs '(btn "btn-primary" "active")
                                               '((href "/about")))
                        #:expect '((@ (class "btn btn-primary active")
                                      (href "/about")))))
         (test "constructs a class attribute when classes are supplied"
               (assert-equal #:got (make-sxml-attrs '("heading"))
                             #:expect '((@ (class "heading"))))
               (assert-equal #:got (make-sxml-attrs '(btn "btn-primary"
                                                          "active"))
                             #:expect '((@ (class "btn btn-primary active")))))
         (test "constructs attributes when they are supplied"
               (assert-equal #:got (make-sxml-attrs '()
                                                    '((href "/home")))
                             #:expect '((@ (href "/home"))))
               (assert-equal #:got (make-sxml-attrs '()
                                                    '((id "main-header")
                                                      (role "banner")))
                             #:expect '((@ (id "main-header")
                                           (role "banner")))))
         (test "returns empty list when no classes or attributes provided"
               (assert-equal #:got (make-sxml-attrs '())
                             #:expect '())
               (assert-equal #:got (make-sxml-attrs '()
                                                    '())
                             #:expect '())
               (assert-equal #:got (make-sxml-attrs '("")
                                                    '())
                             #:expect '()))))

(define inline-org-suite
  (suite "Inline Org Transformation"
         (test "basic inline attributes"
               (assert-equal #:got (inline-org->html (inline-org->ast "*bold*"))
                             #:expect "<strong>bold</strong>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "/italic/"))
                             #:expect "<em>italic</em>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "Unsure if Option A/B"))
                             #:expect "Unsure if Option A/B")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "cron job / shepherd timer that does git fetch --all for work and =~/hacking="))
                #:expect
                "cron job / shepherd timer that does git fetch --all for work and <code>~/hacking</code>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "_underlined_"))
                             #:expect "<u>underlined</u>")
               (assert-equal #:got (inline-org->html (inline-org->ast "~code~"))
                             #:expect "<code>code</code>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "=verbatim="))
                             #:expect "<code>verbatim</code>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "[[https://gnu.org][GNU]]"))
                             #:expect "<a href=\"https://gnu.org\">GNU</a>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "[[https://gnu.org]]"))
                             #:expect
                             "<a href=\"https://gnu.org\">https://gnu.org</a>"))
         (test "nested inline markup"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "*bold with /italic/*"))
                             #:expect
                             "<strong>bold with <em>italic</em></strong>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "_underlined [[https://gnu.org][link]]_"))
                #:expect
                "<u>underlined <a href=\"https://gnu.org\">link</a></u>"))))

(define block-org-suite
  (suite "Block Org AST & Document Parsing"
         (test "headings AST transformation"
               (assert-equal #:got (org->html "* Level 1")
                             #:expect "<h1>Level 1</h1>")
               (assert-equal #:got (org->html "** Level 2")
                             #:expect "<h2>Level 2</h2>")
               (assert-equal #:got (org->html "*** Level 3")
                             #:expect "<h3>Level 3</h3>"))
         (test "paragraph transformation"
               (assert-equal #:got (org->html
                                    "A simple paragraph with *bold* text.")
                #:expect
                "<p>A simple paragraph with <strong>bold</strong> text.</p>"))
         (test "special blocks: quote, example, src, export, separator"
               (assert-equal #:got (org->html
                                              "#+BEGIN_QUOTE\nQuoted *text*\n#+END_QUOTE")
                #:expect
                "<blockquote>Quoted <strong>text</strong></blockquote>")
               (assert-equal #:got (org->html "#+BEGIN_EXAMPLE
*literal* code
#+END_EXAMPLE")
                             #:expect "<pre>*literal* code\n</pre>")
               (assert-equal #:got (org->html "#+BEGIN_SRC scheme
(define x 10)
#+END_SRC")
                             #:expect
                             "<pre class=\"scheme\">(define x 10)\n</pre>")
               (assert-equal #:got (string-trim-both (org->html
                                                      "#+BEGIN_EXPORT html
<div>Raw content</div>
#+END_EXPORT"))
                             #:expect "<div>Raw content</div>")
               (assert-equal #:got (org->html "---------------")
                             #:expect "<hr />"))))

(define org-table-suite
  (suite "Org Table Transformation"
         (test "table rendering with header and body"
               (assert-equal #:got (org->html sample-org-table)
                #:expect
                "<table><thead><tr><th> Name  </th><th> Age </th><th> City   </th></tr></thead><tbody><tr><td> Alice </td><td> 30  </td><td> London </td></tr></tbody><tbody><tr><td> Bob   </td><td> 25  </td><td> Berlin </td></tr></tbody></table>"))
         (test "table error resilience"
               (assert-no-error (lambda ()
                                  (org->html sample-org-table))))))

(define parameterized-classes-suite
  (suite "Class Parameter Customization"
         (test "customizing heading classes dynamically"
               (parameterize ((heading-classes (lambda (lvl)
                                                 (case lvl
                                                   ((1)
                                                    '("text-3xl" "font-bold"))
                                                   ((2)
                                                    '("text-xl"))
                                                   (else '("text-base"))))))
                 (assert-equal #:got (org->html "* Main Title")
                  #:expect "<h1 class=\"text-3xl font-bold\">Main Title</h1>")
                 (assert-equal #:got (org->html "** Section")
                               #:expect "<h2 class=\"text-xl\">Section</h2>")))
         (test "customizing inline markup classes"
               (parameterize ((bold-classes '("font-bold" "text-red-500"))
                              (italic-classes '("italic"))
                              (link-classes '("underline" "text-blue-600")))
                 (assert-equal #:got (inline-org->html (inline-org->ast
                                                        "*styled bold*"))
                  #:expect
                  "<strong class=\"font-bold text-red-500\">styled bold</strong>")
                 (assert-equal #:got (inline-org->html (inline-org->ast
                                                        "[[/path][link]]"))
                  #:expect
                  "<a class=\"underline text-blue-600\" href=\"/path\">link</a>")))
         (test "customizing block element classes"
               (parameterize ((paragraph-classes '("leading-relaxed" "mb-4"))
                              (code-block-classes '("bg-gray-900" "p-4"))
                              (unordered-list-classes '("list-disc" "ml-6")))
                 (assert-equal #:got (org->html "Paragraph text.")
                  #:expect
                  "<p class=\"leading-relaxed mb-4\">Paragraph text.</p>")
                 (assert-equal #:got (org->html
                                                "#+BEGIN_SRC python\nprint(1)\n#+END_SRC")
                  #:expect
                  "<pre class=\"python bg-gray-900 p-4\">print(1)</pre>")
                 (assert-equal #:got (org->html "- item")
                  #:expect "<ul class=\"list-disc ml-6\"><li>item</li></ul>")))))

(define sxml-rendering-suite
  (suite "SXML to HTML String Rendering"
         (test "rendering *TOP* wrapped node tree"
               (assert-equal #:got (sxml->html-string '(*TOP* (p "Hello")
                                                              (p "World")))
                             #:expect "<p>Hello</p><p>World</p>"))
         (test "rendering raw HTML nodes verbatim without escaping"
               (assert-equal #:got (sxml->html-string '(raw-html
                                                        "<span>Raw</span>"))
                             #:expect "<span>Raw</span>"))
         (test "rendering single element vs list of elements"
               (assert-equal #:got (sxml->html-string '(span "text"))
                             #:expect "<span>text</span>")
               (assert-equal #:got (sxml->html-string '((h1 "Title")
                                                        (p "Body")))
                             #:expect "<h1>Title</h1><p>Body</p>"))))

(define org-to-html-suite
  (suite "Full Org-to-HTML Integration"
         (test "sample org text parsing without errors"
               (assert-no-error (lambda ()
                                  (org->html sample-org-text))))
         (test "sample org blocks parsing without errors"
               (assert-no-error (lambda ()
                                  (org->html sample-org-blocks))))))

(define list-to-html-suite
  (suite "List transformations"
         (test "unordered and ordered lists transformation"
               (assert-equal #:got (org->html "- Item 1\n- Item 2")
                             #:expect
                             "<ul><li>Item 1</li><li>Item 2</li></ul>")
               (assert-equal #:got (org->html "1) First\n2) Second")
                             #:expect "<ol><li>First</li><li>Second</li></ol>"))
         (test "nested unordered and nested ordered lists transformation"
               (assert-equal #:got (org->html
                                              "- Item 1\n  - Item 1.1\n  - Item 1.2")
                #:expect
                "<ul><li>Item 1<ul><li>Item 1.1</li><li>Item 1.2</li></ul></li></ul>")
               (assert-equal #:got (org->html "1) Item 1
  1.1) Item 1.1
  1.2) Item 1.2")
                #:expect
                "<ol><li>Item 1<ol><li>Item 1.1</li><li>Item 1.2</li></ol></li></ol>"))
         (test "description list transformation"
               (assert-equal #:got (org->html "- Term :: Definition text")
                             #:expect
                             "<dl><dt>Term</dt><dd>Definition text</dd></dl>"))
         (test "nested description list transformation"
               (assert-equal #:got (org->html "- Term :: Definition text
  - Term2 :: Def2")
                #:expect
                "<dl><dt>Term</dt><dd>Definition text<dl><dt>Term2</dt><dd>Def2</dd></dl></dd></dl>"))))

(define inline-url-suite
  (suite "Inline Raw URL Transformation"
         (test "supports all HTTP and file transfer protocols"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "http://example.org"))
                #:expect
                "<a href=\"http://example.org\">http://example.org</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "https://example.org"))
                #:expect
                "<a href=\"https://example.org\">https://example.org</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "ftp://example.org/file.txt"))
                #:expect
                "<a href=\"ftp://example.org/file.txt\">ftp://example.org/file.txt</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "ftps://example.org/file.txt"))
                #:expect
                "<a href=\"ftps://example.org/file.txt\">ftps://example.org/file.txt</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "file:///home/user/document.org"))
                #:expect
                "<a href=\"file:///home/user/document.org\">file:///home/user/document.org</a>"))

         (test "supports remote access and network protocols"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "ssh://git@example.org/repo"))
                #:expect
                "<a href=\"ssh://git@example.org/repo\">ssh://git@example.org/repo</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "sftp://example.org/path/to/file"))
                #:expect
                "<a href=\"sftp://example.org/path/to/file\">sftp://example.org/path/to/file</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "git://example.org/project.git"))
                #:expect
                "<a href=\"git://example.org/project.git\">git://example.org/project.git</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "gopher://example.org/1/hello"))
                #:expect
                "<a href=\"gopher://example.org/1/hello\">gopher://example.org/1/hello</a>"))

         (test "supports WebSocket protocols"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "ws://example.org/socket"))
                #:expect
                "<a href=\"ws://example.org/socket\">ws://example.org/socket</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "wss://example.org/socket"))
                #:expect
                "<a href=\"wss://example.org/socket\">wss://example.org/socket</a>"))

         (test "supports communication protocols"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "tel:+31612345678"))
                #:expect "<a href=\"tel:+31612345678\">tel:+31612345678</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "mailto:jjbigorra@gmail.com"))
                #:expect
                "<a href=\"mailto:jjbigorra@gmail.com\">mailto:jjbigorra@gmail.com</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "irc://irc.example.org/#emacs"))
                #:expect
                "<a href=\"irc://irc.example.org/#emacs\">irc://irc.example.org/#emacs</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "ircs://irc.example.org/#emacs"))
                #:expect
                "<a href=\"ircs://irc.example.org/#emacs\">ircs://irc.example.org/#emacs</a>"))

         (test "supports URLs with paths, queries, fragments and ports"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "https://example.org:8443/path/to/page"))
                #:expect
                "<a href=\"https://example.org:8443/path/to/page\">https://example.org:8443/path/to/page</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "https://example.org/search?q=org-mode&lang=en"))
                #:expect
                "<a href=\"https://example.org/search?q=org-mode&amp;lang=en\">https://example.org/search?q=org-mode&amp;lang=en</a>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "https://example.org/page#section"))
                #:expect
                "<a href=\"https://example.org/page#section\">https://example.org/page#section</a>"))

         (test "URLs can appear inside ordinary prose"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "Visit https://example.org for more information."))
                #:expect
                "Visit <a href=\"https://example.org\">https://example.org</a> for more information.")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "Clone git://example.org/project.git and build it."))
                #:expect
                "Clone <a href=\"git://example.org/project.git\">git://example.org/project.git</a> and build it."))

         (test "URLs stop at whitespace"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "See https://example.org here."))
                #:expect
                "See <a href=\"https://example.org\">https://example.org</a> here.")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "See https://example.org\tfor details."))
                #:expect
                "See <a href=\"https://example.org\">https://example.org</a>\tfor details."))

         (test "URLs stop at a newline"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "https://example.org\nhttps://gnu.org"))
                #:expect
                "<a href=\"https://example.org\">https://example.org</a>"))

         (test "URLs coexist with inline Org markup"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "*Visit https://example.org*"))
                #:expect
                "<strong>Visit <a href=\"https://example.org\">https://example.org</a></strong>")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "See https://example.org and *bold* text."))
                #:expect
                "See <a href=\"https://example.org\">https://example.org</a> and <strong>bold</strong> text."))

         (test "raw URLs and Org links coexist"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "Visit https://example.org or [[https://gnu.org][GNU]]."))
                #:expect
                "Visit <a href=\"https://example.org\">https://example.org</a> or <a href=\"https://gnu.org\">GNU</a>."))

         (test "does not treat unsupported schemes as raw URLs"
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "custom://example.org"))
                             #:expect "custom://example.org")

               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "javascript:alert(1)"))
                             #:expect "javascript:alert(1)"))))

(define extended-inline-suite
  (suite "Extended inline markup"
         (test "strikethrough, superscript and subscript"
               (assert-equal #:got (inline-org->html (inline-org->ast "+struck+"))
                             #:expect "<del>struck</del>")
               (assert-equal #:got (inline-org->html (inline-org->ast "x^{2}"))
                             #:expect "x<sup>2</sup>")
               (assert-equal #:got (inline-org->html (inline-org->ast "H_{2}O"))
                             #:expect "H<sub>2</sub>O"))
         (test "forced line break and escaped markup"
               (assert-equal #:got (inline-org->html (inline-org->ast "line\\\\"))
                             #:expect "line<br />")
               (assert-equal #:got (inline-org->html (inline-org->ast "\\+"))
                             #:expect "+"))
         (test "footnote reference, definition and inline form"
               (assert-equal #:got (inline-org->html (inline-org->ast "see[fn:1]"))
                #:expect
                "see<sup class=\"footnote-ref\"><a href=\"#fn-1\" id=\"fnref-1\">1</a></sup>")
               (assert-equal #:got (inline-org->html (inline-org->ast "[fn:1: note]"))
                #:expect
                "<span class=\"footnote-def\" id=\"fn-1\"><sup class=\"footnote-label\">1</sup> note</span>")
               (assert-equal #:got (inline-org->html (inline-org->ast "[fn::note]"))
                #:expect
                "<sup class=\"footnote-inline\">note</sup>"))))

(define extended-block-suite
  (suite "Extended block elements"
         (test "keywords and comments"
               (assert-equal #:got (org->html "#+TITLE: Hello\n")
                             #:expect "<div>TITLE: Hello</div>")
               (assert-equal #:got (org->html "# a comment\n")
                             #:expect "<!-- a comment -->")
               (assert-equal #:got (org->html
                                    "#+BEGIN_COMMENT\nhidden\n#+END_COMMENT\n")
                             #:expect "<!-- hidden\n-->"))
         (test "fixed-width, verse and center blocks"
               (assert-equal #:got (org->html ": fixed\n")
                             #:expect "<pre>fixed\n</pre>")
               (assert-equal #:got (org->html
                                    "#+BEGIN_VERSE\none\ntwo\n#+END_VERSE\n")
                             #:expect "<p>one<br />two</p>")
               (assert-equal #:got (org->html
                                    "#+BEGIN_CENTER\nmid\n#+END_CENTER\n")
                             #:expect "<div><p>mid</p></div>"))
         (test "checkbox list items"
               (assert-equal #:got (org->html "- [ ] todo\n- [X] done\n")
                #:expect
                "<ul><li><input type=\"checkbox\" disabled=\"disabled\" />todo</li><li><input type=\"checkbox\" disabled=\"disabled\" checked=\"checked\" />done</li></ul>"))))

(define heading-metadata-suite
  (suite "Heading metadata"
         (test "TODO keyword, priority and tags render"
               (assert-equal #:got (org->html "* TODO [#A] A Task :work:urgent:\n")
                #:expect
                "<h1 class=\"org-todo org-todo-todo\"><span class=\"org-todo\">TODO</span><span class=\"org-priority\">[#A]</span>A Task<span class=\"org-tags\"><span class=\"org-tag\">work</span><span class=\"org-tag\">urgent</span></span></h1>"))
         (test "plain headings are unchanged"
               (assert-equal #:got (org->html "* Plain\n")
                             #:expect "<h1>Plain</h1>"))))

(define feature-fixture-suite
  (suite "Feature fixture"
         (test "the whole fixture renders without error"
               (assert-no-error (lambda ()
                                  (org->html sample-org-features))))
         (test "raw HTML export passes through untouched"
               (assert-contains #:expect (org->html sample-org-features)
                                #:got "<div class=\"raw\">Raw HTML</div>"))
         (test "tables still render their header"
               (assert-matches #:pattern "<th> Name </th>"
                               #:got (org->html sample-org-features)))))

(define (spec)
  (veritas-run make-sxml-attrs-suite
               inline-org-suite
               block-org-suite
               org-table-suite
               parameterized-classes-suite
               sxml-rendering-suite
               org-to-html-suite
               inline-url-suite
               list-to-html-suite
               extended-inline-suite
               extended-block-suite
               heading-metadata-suite
               feature-fixture-suite))
