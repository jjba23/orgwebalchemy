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

(define-module (orgwebalchemy org-to-html-test)
  #:use-module ((srfi srfi-64)
                #:hide (define-test))
  #:use-module (orgwebalchemy test-utils)
  #:use-module (orgwebalchemy org-to-html))

(define-test test-org-to-html
             (test-group "string-ops"
                         (test-equal "string-drop-first-last normal case"
                                     "ell"
                                     (string-drop-first-last "hello"))
                         (test-equal "string-drop-first-last single char" "a"
                                     (string-drop-first-last "a"))
                         (test-equal "string-drop-first-last two chars" "ab"
                                     (string-drop-first-last "ab"))
                         (test-equal "string-drop-first-last empty string" ""
                                     (string-drop-first-last ""))

                         (test-equal "string-drop-first-last-line normal case"
                          "middle line"
                          (string-drop-first-last-line
                                                       "first line\nmiddle line\nlast line"))
                         (test-equal "string-drop-first-last-line single line"
                          ""
                          (string-drop-first-last-line "single line"))
                         (test-equal "string-drop-first-last-line two lines"
                                     ""
                                     (string-drop-first-last-line
                                      "first line\nlast line"))
                         (test-equal "string-drop-first-last-line empty input"
                          ""
                          (string-drop-first-last-line "")))

             (test-group "inline code"
                         (test-equal "org-to-html inline code parsing"
                          "This is <code class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg py-1 px-2\">test</code> code."
                          (org-to-html "This is ~test~ code.")))

             (test-group "code block"

                         (test-equal "org-to-html code block parsing"
                          "<pre class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">some code
inside block</pre>"
                          (org-to-html "#+begin_src
some code
inside block
#+end_src"))

                         (test-equal
                          "org-to-html mixed inline and block parsing"
                          "Here is <code class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg py-1 px-2\">inline</code> code and a block: <pre class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">code block</pre>"
                          (org-to-html
                           "Here is ~inline~ code and a block: #+begin_src
code block
#+end_src")))

             (test-group "org-to-html"
                         (test-equal "org-to-html no matches"
                                     "Just some text with no code."
                                     (org-to-html
                                      "Just some text with no code."))

                         (test-equal "org-to-html inline italic parsing"
                                     "This is <em class=\"\">test</em> code."
                                     (org-to-html "This is /test/ code."))

                         (test-equal "org-to-html inline strong parsing"
                          "This is <strong class=\"\">test</strong> code."
                          (org-to-html "This is *test* code."))

                         (test-equal "org-to-html inline combined formatting"
                          "This is <em class=\"\"><strong class=\"\">test</strong></em> code."
                          (org-to-html "This is /*test*/ code."))

                         (test-equal
                          "org-to-html consecutive inline formatting"
                          "<strong class=\"\">bold</strong> <em class=\"\">italic</em> <code class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg py-1 px-2\">code</code>"
                          (org-to-html "*bold* /italic/ ~code~"))

                         (test-equal
                          "org-to-html mixed block and inline formatting"
                          "Inline <code class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg py-1 px-2\">code</code> before block: <pre class=\"bg-stone-200/75 dark:bg-stone-700/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">block code</pre> after block."
                          (org-to-html
                           "Inline ~code~ before block: #+begin_src
block code
#+end_src after block."))

                         (test-equal "org-to-html multiline inline formatting"
                          "This is <em class=\"\">multi
line</em> italic text."
                          (org-to-html "This is /multi\nline/ italic text."))

                         (test-equal "org-to-html inline link parsing"
                          "This is <a class=\"text-blue-600 hover:text-blue-800 dark:text-blue-400 hover:dark:text-blue-300 hover:underline font-bold\" href=\"https://www.google.com\">some link</a> code."
                          (org-to-html
                           "This is [[https://www.google.com][some link]] code."))

                         (test-equal
                          "org-to-html inline link parsing of same desc"
                          "This is <a class=\"text-blue-600 hover:text-blue-800 dark:text-blue-400 hover:dark:text-blue-300 hover:underline font-bold\" href=\"https://www.google.com\">https://www.google.com</a> code."
                          (org-to-html
                           "This is [[https://www.google.com][https://www.google.com]] code."))

                         (test-equal
                          "org-to-html inline link parsing of same desc"
                          "Created GitHub ticket here: <a class=\"text-blue-600 hover:text-blue-800 dark:text-blue-400 hover:dark:text-blue-300 hover:underline font-bold\" href=\"https://github.com/flatpak/flatpak/issues/6135\">https://github.com/flatpak/flatpak/issues/6135</a>"
                          (org-to-html
                           "Created GitHub ticket here: [[https://github.com/flatpak/flatpak/issues/6135][https://github.com/flatpak/flatpak/issues/6135]]"))

                         (test-equal "org-to-html parses hr properly"
                          "
some section
<hr class=\"my-8 text-stone-500 dark:text-stone-500\"/>
then another
"
                          (org-to-html
                                       "\nsome section\n\n---------\n\nthen another\n"))))

