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
  #:use-module (veritas veritas)
  #:use-module (orgwebalchemy peg)
  #:use-module (orgwebalchemy html))

(define sample-org-text
  "* Top Level Heading
This is a standard paragraph.
It sits right under the heading.

** A Subheading
- First list item
- Second list item

*** An ordered subheading
1) yes buddy
2) another item

** Another
- key :: val
- desc :: conte

/In Vino Veritas/ but /In Aqua Sanitas/

#+BEGIN_SRC scheme
(define (hello)
  (display \"Hello World!\"))
#+END_SRC")

(define sample-org-blocks
  "
#+begin_example
* Verbatim content
- Do not parse
#+end_example

#+begin_quote
IN VINO VERITAS IN AQUA SANITAS
#+end_quote

---------------

#+begin_export html
<strong><p>HELLO WORLD</p></strong>
#+end_export
")

(define sample-org-table
  "
| Name  | Age | City   |
|-------+-----+--------|
| Alice | 30  | London |
| Bob   | 25  | Berlin |
")

(define org-table-suite
  (suite "Org tables"
         (test "Org table parsing"
               (assert-no-error (lambda ()
                                  (display (org->html sample-org-table)))))))

(define org-to-html-suite
  (suite "Org to HTML"
         (test "Org mode parsing"
               (assert-no-error (lambda ()
                                  (display (org->html sample-org-text)))))
         (test "Org blocks parsing"
               (assert-no-error (lambda ()
                                  (display (org->html sample-org-blocks)))))))

(define inline-org-suite
  (suite "inline org"
         (test "basic inline attributes"
               (assert-equal #:got (inline-org->html (inline-org->ast "*bold*"))
                             #:expect "<strong>bold</strong>")
               (assert-equal #:got (inline-org->html (inline-org->ast
                                                      "/italic/"))
                             #:expect "<em>italic</em>")
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
                             "<a href=\"https://gnu.org\">https://gnu.org</a>"))))

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
         (test "return empty list when no classes or attributes provided"
               (assert-equal #:got (make-sxml-attrs '())
                             #:expect '())
               (assert-equal #:got (make-sxml-attrs '()
                                                    '())
                             #:expect '())
               (assert-equal #:got (make-sxml-attrs '("")
                                                    '())
                             #:expect '()))))

(define (spec)
  (veritas-run org-to-html-suite make-sxml-attrs-suite inline-org-suite
               org-table-suite))
