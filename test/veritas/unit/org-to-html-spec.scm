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

(define-module (veritas unit org-to-html-spec)
  #:use-module (veritas veritas)
  #:use-module (orgwebalchemy org-to-html))

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

(define sample-org-inline-text
  "This is *bold*, /italic/, ~code~, =verbatim= and _underlined_.")

(define sample-org-links
  "* Useful Resources and Links

Org mode makes it very easy to manage references. Here is a standard link with a description pointing to a search engine: [[https://duckduckgo.com][DuckDuckGo]].

Sometimes you just want to drop a URL directly into your document. Here is a link without a description: [[https://orgmode.org/]]

** Local Files and Internal Links
You can also link to local files on your system.
- With description: [[file:~/.emacs.d/init.el][My Emacs config]]
- Without description: [[file:~/Documents/notes.txt]]

If you want to jump to the top heading of this document, you can click this internal link: [[*Useful Resources and Links][Jump to top]].
")

(define org-to-html-suite
  (suite "Org to HTML"
         (test "Org mode parsing"
               (assert-no-error (lambda ()
                                  (newline)
                                  (display (org->html sample-org-text)))))
         (test "Inline Org mode parsing"
               (assert-no-error (lambda ()
                                  (newline)
                                  (display (inline->html (inline-org->ast
                                                          sample-org-inline-text))))))
         (test "Inline Org link parsing"
               (assert-no-error (lambda ()
                                  (newline)
                                  (display (org->html sample-org-links)))))))

(define (spec)
  (veritas-run org-to-html-suite))
