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

(define-module (orgwebalchemy org-to-html)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:export (org-to-html org-code-pattern
                        org-code-replacement
                        org-code-replacement-procedure
                        org-code-block-pattern
                        org-code-block-replacement
                        org-code-block-replacement-procedure
                        org-em-patern
                        org-em-replacement
                        org-em-replacement-procedure
                        org-hr-patern
                        org-hr-replacement
                        org-hr-replacement-procedure
                        org-strong-pattern
                        org-strong-replacement
                        org-strong-replacement-procedure
                        org-link-pattern
                        org-link-replacement
                        org-link-replacement-procedure
                        string-drop-first-last
                        string-drop-first-last-n
                        string-drop-first-last-line))

(define (string-drop-first-last s)
  (if (> (string-length s) 2)
      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (string-drop-first-last-n s n)
  (if (> (string-length s) 2)
      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define (string-drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

(define org-code-pattern
  (make-parameter "~([^~]+)~"))

(define org-code-replacement
  (make-parameter "<code class=\"~a\">~a</code>"))

(define org-code-block-pattern
  (make-parameter "#\\+begin_src.*\n.*#\\+end_src"))

(define org-code-block-replacement
  (make-parameter "<pre class=\"~a\">~a</pre>"))

(define org-em-pattern
  (make-parameter "[[:space:]]/([^/<>:]+)/[[:space:]]"))

;; space is important for italic replacement
;; due to links
(define org-em-replacement
  (make-parameter " <em class=\"~a\">~a</em> "))

(define org-hr-pattern
  (make-parameter "\n---*\n"))

(define org-hr-replacement
  (make-parameter "<hr class=\"~a\"/>"))

(define org-strong-pattern
  (make-parameter "\\*([^*]+)\\*"))

(define org-strong-replacement
  (make-parameter "<strong class=\"~a\">~a</strong>"))

(define org-link-pattern
  (make-parameter "\\[\\[.*\\]\\]"))

(define org-link-replacement
  (make-parameter "<a class=\"~a\" href=\"~a\">~a</a>"))

(define org-code-block-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (format #f
              (org-code-block-replacement)
              (string-join (map symbol->string classes) " ")
              (string-drop-first-last-line (match:substring m))))))

(define org-em-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (cond
        ((string-contains (match:substring m) "://")
         (string-drop-first-last (string-trim-both (match:substring m))))
        (else (format #f
                      (org-em-replacement)
                      (string-join (map symbol->string classes) " ")
                      (string-drop-first-last (string-trim-both (match:substring
                                                                 m)))))))))

(define org-strong-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (format #f
              (org-strong-replacement)
              (string-join (map symbol->string classes) " ")
              (string-drop-first-last (match:substring m))))))

(define org-link-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (let* ((l (string-drop-first-last (match:substring m)))
             (split-l (string-split (string-replace-substring l "][" "|") #\|))
             (link (string-drop (car split-l) 1))
             (desc (string-drop-right (car (cdr split-l)) 1)))
        (format #f
                (org-link-replacement)
                (string-join (map symbol->string classes) " ") link desc)))))

(define org-code-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (format #f
              (org-code-replacement)
              (string-join (map symbol->string classes) " ")
              (string-drop-first-last (match:substring m))))))

(define org-hr-replacement-procedure
  (lambda (classes)
    (lambda (m)
      (format #f
              (org-hr-replacement)
              (string-join (map symbol->string classes) " ")))))

(define* (org-to-html org
                      #:key (link-classes '(text-blue-600 hover:text-blue-800
                                            dark:text-blue-400
                                            hover:dark:text-blue-300
                                            hover:underline font-bold))
                      (code-classes '(bg-stone-200/75 dark:bg-stone-700/75
                                                      rounded-lg py-1 px-2))
                      (em-classes '())
                      (strong-classes '())
                      (hr-classes '(my-8 text-stone-500 dark:text-stone-500))
                      (code-block-classes '(bg-stone-200/75
                                            dark:bg-stone-700/75
                                            rounded-lg
                                            my-3
                                            py-2
                                            px-3
                                            whitespace-pre
                                            overflow-x-scroll)))
  (let* ((with-parsed-link (regexp-substitute/global #f
                                                     (org-link-pattern)
                                                     org
                                                     'pre
                                                     (org-link-replacement-procedure
                                                      link-classes)
                                                     'post))
         (with-parsed-code (regexp-substitute/global #f
                                                     (org-code-pattern)
                                                     with-parsed-link
                                                     'pre
                                                     (org-code-replacement-procedure
                                                      code-classes)
                                                     'post))
         (with-parsed-italic (regexp-substitute/global #f
                                                       (org-em-pattern)
                                                       with-parsed-code
                                                       'pre
                                                       (org-em-replacement-procedure
                                                        em-classes)
                                                       'post))
         (with-parsed-strong (regexp-substitute/global #f
                                                       (org-strong-pattern)
                                                       with-parsed-italic
                                                       'pre
                                                       (org-strong-replacement-procedure
                                                        strong-classes)
                                                       'post))
         (with-parsed-hr (regexp-substitute/global #f
                                                   (org-hr-pattern)
                                                   with-parsed-strong
                                                   'pre
                                                   (org-hr-replacement-procedure
                                                    hr-classes)
                                                   'post))
         (with-parsed-code-block (regexp-substitute/global #f
                                                           (org-code-block-pattern)
                                                           with-parsed-hr
                                                           'pre
                                                           (org-code-block-replacement-procedure
                                                            code-block-classes)
                                                           'post)))
    with-parsed-code-block))
