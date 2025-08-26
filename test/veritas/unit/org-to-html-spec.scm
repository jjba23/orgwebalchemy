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

(define org-to-html-suite
  (suite "Org to HTML"
         (test "string-ops"
               (assert-equal #:name "string-drop-first-last normal case"
                             #:expect "ell"
                             #:got (string-drop-first-last "hello"))
               (assert-equal #:name "string-drop-first-last single char"
                             #:expect "a"
                             #:got (string-drop-first-last "a"))
               (assert-equal #:name "string-drop-first-last two chars"
                             #:expect "ab"
                             #:got (string-drop-first-last "ab"))
               (assert-equal #:name "string-drop-first-last empty string"
                             #:expect ""
                             #:got (string-drop-first-last ""))

               (assert-equal #:name "string-drop-first-last-line normal case"
                             #:expect "middle line"
                             #:got (string-drop-first-last-line
                                                                "first line\nmiddle line\nlast line"))
               (assert-equal #:name "string-drop-first-last-line single line"
                             #:expect ""
                             #:got (string-drop-first-last-line "single line"))
               (assert-equal #:name "string-drop-first-last-line two lines"
                             #:expect ""
                             #:got (string-drop-first-last-line
                                                                "first line\nlast line"))
               (assert-equal #:name "string-drop-first-last-line empty input"
                             #:expect ""
                             #:got (string-drop-first-last-line "")))

         (test "inline code"
               (assert-equal #:name "org-to-html inline code parsing"
                #:expect
                "This is <code class=\"bg-stone-200/75 dark:bg-stone-700/75 px-2 py-1 rounded-lg\">test</code> code."
                #:got (org-to-html "This is ~test~ code.")))

         (test "code block"

               (assert-equal #:name "org-to-html code block parsing"
                #:expect
                "<pre class=\"bg-stone-200/75 dark:bg-stone-700/75 my-3 overflow-x-scroll px-3 py-2 rounded-lg whitespace-pre\">some code
inside block</pre>"
                #:got (org-to-html "#+begin_src
some code
inside block
#+end_src"))

               (assert-equal #:name
                             "org-to-html mixed inline and block parsing"
                             #:expect (string-append "Here is "
                                       "<code class=\"bg-stone-200/75 dark:bg-stone-700/75 px-2 py-1 rounded-lg\">inline</code>"
                                       " code and a block: "
                                       "<pre class=\"bg-stone-200/75 dark:bg-stone-700/75 my-3 overflow-x-scroll px-3 py-2 rounded-lg whitespace-pre\">code block</pre>")
                             #:got (org-to-html
                                    "Here is ~inline~ code and a block: #+begin_src
code block
#+end_src")))

         (test "org-to-html"
               (assert-equal #:name "org-to-html no matches"
                             #:expect "Just some text with no code."
                             #:got (org-to-html "Just some text with no code."))

               (assert-equal #:name "org-to-html inline italic parsing"
                             #:expect "This is <em class=\"\">test</em> code."
                             #:got (org-to-html "This is /test/ code."))

               (assert-equal #:name "org-to-html inline strong parsing"
                             #:expect
                             "This is <strong class=\"\">test</strong> code."
                             #:got (org-to-html "This is *test* code."))

               (assert-equal #:name "org-to-html inline combined formatting"
                #:expect
                "This is <em class=\"\"><strong class=\"\">test</strong></em> code."
                #:got (org-to-html "This is /*test*/ code."))

               (assert-equal #:name
                "org-to-html consecutive inline formatting"
                #:expect
                "<strong class=\"\">bold</strong> <em class=\"\">italic</em> <code class=\"bg-stone-200/75 dark:bg-stone-700/75 px-2 py-1 rounded-lg\">code</code>"
                #:got (org-to-html "*bold* /italic/ ~code~"))

               (assert-equal #:name
                "org-to-html mixed block and inline formatting"
                #:expect
                "Inline <code class=\"bg-stone-200/75 dark:bg-stone-700/75 px-2 py-1 rounded-lg\">code</code> before block: <pre class=\"bg-stone-200/75 dark:bg-stone-700/75 my-3 overflow-x-scroll px-3 py-2 rounded-lg whitespace-pre\">block code</pre> after block."
                #:got (org-to-html "Inline ~code~ before block: #+begin_src
block code
#+end_src after block."))

               (assert-equal #:name "org-to-html multiline inline formatting"
                             #:expect "This is <em class=\"\">multi
line</em> italic text."
                             #:got (org-to-html
                                                "This is /multi\nline/ italic text."))

               (assert-equal #:name "org-to-html inline link parsing"
                #:expect
                "This is <a class=\"dark:text-blue-400 font-bold hover:dark:text-blue-300 hover:text-blue-800 hover:underline text-blue-600\" href=\"https://www.google.com\">some link</a> code."
                #:got (org-to-html
                       "This is [[https://www.google.com][some link]] code."))

               (assert-equal #:name
                "org-to-html inline link parsing of same desc"
                #:expect
                "This is <a class=\"dark:text-blue-400 font-bold hover:dark:text-blue-300 hover:text-blue-800 hover:underline text-blue-600\" href=\"https://www.google.com\">https://www.google.com</a> code."
                #:got (org-to-html
                       "This is [[https://www.google.com][https://www.google.com]] code."))

               (assert-equal #:name
                             "org-to-html inline link parsing of same desc"
                             #:expect (string-append
                                       "Created GitHub ticket here: "
                                       "<a class=\"dark:text-blue-400 font-bold hover:dark:text-blue-300 hover:text-blue-800 hover:underline text-blue-600\" href=\"https://github.com/flatpak/flatpak/issues/6135\">https://github.com/flatpak/flatpak/issues/6135</a>")
                             #:got (org-to-html
                                    "Created GitHub ticket here: [[https://github.com/flatpak/flatpak/issues/6135][https://github.com/flatpak/flatpak/issues/6135]]"))

               (assert-equal #:name "org-to-html parses hr properly"
                #:expect
                "
some section
<hr class=\"dark:text-stone-500 my-8 text-stone-500\"/>
then another
"
                #:got (org-to-html
                                   "\nsome section\n\n---------\n\nthen another\n")))))

(define (spec)
  (veritas-run org-to-html-suite))
