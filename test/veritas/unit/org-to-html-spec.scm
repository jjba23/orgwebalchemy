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

(define org-to-html-suite
  (suite "Org to HTML"
         (test "Org mode parsing"
               (assert-no-error (lambda ()
                                  (display "\n(org-to-html sample-org-text)\n")
                                  (display (org-to-html sample-org-text))))
               (assert-equal #:got (org-to-html
                                    "This is some ~technical~ thing")
                             #:expect
                             "This is some <code>technical</code> thing"))))

(define (spec)
  (veritas-run org-to-html-suite))
