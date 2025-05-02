;;; orgwebalchemy

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

(define-module (orgwebalchemy parser)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv))

(define-public (string-drop-first-last s)
  (if (> (string-length s) 2)
      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define-public (string-drop-first-last-n s n)
  (if (> (string-length s) 2)

      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define-public (string-drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

(define-public org-code-pattern
  "~([^~]+)~")

(define-public org-code-replacement
  "<code class=\"~a\">~a</code>")

(define-public (org-code-replacement-procedure code-classes)
  (lambda (m)
    (format #f org-code-replacement
            (string-join (map symbol->string code-classes) " ")
            (string-drop-first-last (match:substring m)))))

(define-public org-code-block-pattern
  "#\\+begin_src.*\n.*#\\+end_src")

(define-public org-code-block-replacement
  "<pre class=\"~a\">~a</pre>")

(define-public (org-code-block-replacement-procedure code-block-classes)
  (lambda (m)
    (format #f org-code-block-replacement
            (string-join (map symbol->string code-block-classes) " ")
            (string-drop-first-last-line (match:substring m)))))

(define-public org-em-pattern
  "[[:space:]]/([^/<>:]+)/[[:space:]]")

(define-public (org-em-replacement-procedure m)
  (cond
    ((string-contains (match:substring m) "://")
     (string-drop-first-last (string-trim-both (match:substring m))))
    (else (format #f org-em-replacement
                  (string-drop-first-last (string-trim-both (match:substring m)))))))

;; space is important for italic replacement
;; due to links
(define-public org-em-replacement
  " <em>~a</em> ")

(define-public org-strong-pattern
  "\\*([^*]+)\\*")

(define-public org-strong-replacement
  "<strong>~a</strong>")

(define-public (org-strong-replacement-procedure m)
  (format #f org-strong-replacement
          (string-drop-first-last (match:substring m))))

(define-public org-link-pattern
  "\\[\\[.*\\]\\]")

(define-public org-link-replacement
  "<a class=\"~a\" href=\"~a\">~a</a>")

(define-public (org-link-replacement-procedure link-classes)
  (lambda (m)
    (let* ((l (string-drop-first-last (match:substring m)))
           (split-l (string-split (string-replace-substring l "][" "|") #\|))
           (link (string-drop (car split-l) 1))
           (desc (string-drop-right (car (cdr split-l)) 1)))
      (format #f org-link-replacement
              (string-join (map symbol->string link-classes) " ") link desc))))

(begin
  (define* (org-to-html org
                        #:key (link-classes '(text-blue-800 font-bold))
                        (code-classes '(bg-stone-200/75 rounded-lg py-1 px-2))
                        (code-block-classes '(bg-stone-200/75 rounded-lg
                                              my-3
                                              py-2
                                              px-3
                                              whitespace-pre
                                              overflow-x-scroll)))
    (let* ((with-parsed-link (regexp-substitute/global #f
                                                       org-link-pattern
                                                       org
                                                       'pre
                                                       (org-link-replacement-procedure
                                                        link-classes)
                                                       'post))
           (with-parsed-code (regexp-substitute/global #f
                                                       org-code-pattern
                                                       with-parsed-link
                                                       'pre
                                                       (org-code-replacement-procedure
                                                        code-classes)
                                                       'post))
           (with-parsed-italic (regexp-substitute/global #f
                                org-em-pattern
                                with-parsed-code
                                'pre
                                org-em-replacement-procedure
                                'post))
           (with-parsed-strong (regexp-substitute/global #f
                                org-strong-pattern
                                with-parsed-italic
                                'pre
                                org-strong-replacement-procedure
                                'post))
           (with-parsed-code-block (regexp-substitute/global #f
                                    org-code-block-pattern
                                    with-parsed-strong
                                    'pre
                                    (org-code-block-replacement-procedure
                                     code-block-classes)
                                    'post)))
      with-parsed-code-block))
  (export org-to-html))

;;
;; module tests
;;
(test-begin "org-to-html")

(test-equal "string-drop-first-last normal case" "ell"
            (string-drop-first-last "hello"))
(test-equal "string-drop-first-last single char" "a"
            (string-drop-first-last "a"))
(test-equal "string-drop-first-last two chars" "ab"
            (string-drop-first-last "ab"))
(test-equal "string-drop-first-last empty string" ""
            (string-drop-first-last ""))

(test-equal "string-drop-first-last-line normal case" "middle line"
            (string-drop-first-last-line "first line\nmiddle line\nlast line"))
(test-equal "string-drop-first-last-line single line" ""
            (string-drop-first-last-line "single line"))
(test-equal "string-drop-first-last-line two lines" ""
            (string-drop-first-last-line "first line\nlast line"))
(test-equal "string-drop-first-last-line empty input" ""
            (string-drop-first-last-line ""))

(test-equal "org-to-html inline code parsing"
 "This is <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">test</code> code."
 (org-to-html "This is ~test~ code."))

(test-equal "org-to-html code block parsing"
 "<pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">some code
inside block</pre>"
 (org-to-html "#+begin_src
some code
inside block
#+end_src"))

(test-equal "org-to-html mixed inline and block parsing"
 "Here is <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">inline</code> code and a block: <pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">code block</pre>"
 (org-to-html "Here is ~inline~ code and a block: #+begin_src
code block
#+end_src"))

(test-equal "org-to-html no matches" "Just some text with no code."
            (org-to-html "Just some text with no code."))

(test-equal "org-to-html inline italic parsing" "This is <em>test</em> code."
            (org-to-html "This is /test/ code."))

(test-equal "org-to-html inline strong parsing"
            "This is <strong>test</strong> code."
            (org-to-html "This is *test* code."))

(test-equal "org-to-html inline combined formatting"
            "This is <em><strong>test</strong></em> code."
            (org-to-html "This is /*test*/ code."))

(test-equal "org-to-html consecutive inline formatting"
 "<strong>bold</strong> <em>italic</em> <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">code</code>"
 (org-to-html "*bold* /italic/ ~code~"))

(test-equal "org-to-html mixed block and inline formatting"
 "Inline <code class=\"bg-stone-200/75 rounded-lg py-1 px-2\">code</code> before block: <pre class=\"bg-stone-200/75 rounded-lg my-3 py-2 px-3 whitespace-pre overflow-x-scroll\">block code</pre> after block."
 (org-to-html "Inline ~code~ before block: #+begin_src
block code
#+end_src after block."))

(test-equal "org-to-html multiline inline formatting"
            "This is <em>multi\nline</em> italic text."
            (org-to-html "This is /multi\nline/ italic text."))

(test-equal "org-to-html inline link parsing"
 "This is <a class=\"text-blue-800 font-bold\" href=\"https://www.google.com\">some link</a> code."
 (org-to-html "This is [[https://www.google.com][some link]] code."))

(test-equal "org-to-html inline link parsing of same desc"
 "This is <a class=\"text-blue-800 font-bold\" href=\"https://www.google.com\">https://www.google.com</a> code."
 (org-to-html
  "This is [[https://www.google.com][https://www.google.com]] code."))

(test-equal "org-to-html inline link parsing of same desc"
 "Created GitHub ticket here: <a class=\"text-blue-800 font-bold\" href=\"https://github.com/flatpak/flatpak/issues/6135\">https://github.com/flatpak/flatpak/issues/6135</a>"
 (org-to-html
  "Created GitHub ticket here: [[https://github.com/flatpak/flatpak/issues/6135][https://github.com/flatpak/flatpak/issues/6135]]"))

(test-end)
