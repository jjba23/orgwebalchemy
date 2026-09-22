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

(define-module (orgwebalchemy html)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
  #:use-module (orgwebalchemy peg)
  #:export (inline-org->html org->html org-ast->html
                             link-classes
                             code-classes
                             code-block-classes
                             italic-classes
                             bold-classes
                             hr-classes))

(define link-classes (make-parameter '()))
(define code-classes (make-parameter '()))
(define code-block-classes (make-parameter '()))
(define italic-classes (make-parameter '()))
(define bold-classes (make-parameter '()))
(define hr-classes (make-parameter '()))

(define (escape-html text)
  (call-with-output-string (lambda (port)
                             (string-for-each (lambda (char)
                                                (display (case char
                                                           ((#\&)
                                                            "&amp;")
                                                           ((#\<)
                                                            "&lt;")
                                                           ((#\>)
                                                            "&gt;")
                                                           ((#\")
                                                            "&quot;")
                                                           ((#\')
                                                            "&#39;")
                                                           (else (string char)))
                                                         port)) text))))

(define (inline-org->html ast)
  (define (render node)
    (match node

      ;; Escaped markup
      (('inline-escape text)
       (escape-html text))

      ;; ~code~
      (('inline-code-content text)
       (format #f "<code>~a</code>"
               (escape-html text)))

      ;; =verbatim=
      (('inline-verbatim-content text)
       (format #f "<code>~a</code>"
               (escape-html text)))

      ;; /italic/
      (('inline-italic-content text)
       (format #f "<em>~a</em>"
               (inline-org->html (inline-org->ast text))))

      ;; *bold*
      (('inline-bold-content text)
       (format #f "<strong>~a</strong>"
               (inline-org->html (inline-org->ast text))))

      ;; _underline_
      (('inline-underline-content text)
       (format #f "<u>~a</u>"
               (inline-org->html (inline-org->ast text))))

      ;; [[URL][description]]
      (('inline-link-with-desc ('link-target-content url)
        ('link-description-content description))
       (format #f "<a href=\"~a\">~a</a>"
               (escape-html url)
               (inline-org->html (inline-org->ast description))))

      ;; [[URL]]
      (('inline-link-no-desc ('link-target-content url))
       (format #f "<a href=\"~a\">~a</a>"
               (escape-html url)
               (escape-html url)))

      ;; Plain text
      (('inline-text text)
       (escape-html text))

      ;; Raw string fallback
      ((? string? text)
       (escape-html text))

      (_ "")))

  (string-join (map render ast) ""))

(define (org-ast->html ast)
  (define (render node)
    (match node
      (('heading ('stars s)
        ('line-content text))
       (let ((lvl (string-length s)))
         (format #f "<h~a>~a</h~a>" lvl text lvl)))
      (('description-list items ...)
       (let ((dl-strings (map (lambda (li)
                                (match li
                                  (('unordered-item ('desc-key key)
                                    ('line-content text))
                                   (format #f "  <dt>~a</dt>\n  <dd>~a</dd>"
                                           (inline-org->html (inline-org->ast
                                                              key))
                                           (inline-org->html (inline-org->ast
                                                              text))))
                                  (_ ""))) items)))
         (format #f "<dl>\n~a\n</dl>"
                 (string-join dl-strings "\n"))))
      (('unordered-list items ...)
       (let ((li-strings (map (lambda (li)
                                (match li
                                  (('unordered-item ('line-content text))
                                   (format #f "  <li>~a</li>"
                                           (inline-org->html (inline-org->ast
                                                              text))))
                                  (_ ""))) items)))
         (format #f "<ul>\n~a\n</ul>"
                 (string-join li-strings "\n"))))
      (('ordered-list items ...)
       (let ((li-strings (map (lambda (li)
                                (match li
                                  (('ordered-item ('line-content text))
                                   (format #f "  <li>~a</li>"
                                           (inline-org->ast text)))
                                  (_ ""))) items)))
         (format #f "<ol>\n~a\n</ol>"
                 (string-join li-strings "\n"))))

      (('paragraph ('line-content text))
       (format #f "<p>~a</p>"
               (inline-org->html (inline-org->ast text))))
      (('src-block ('lang l)
        ('block-content bc))
       (format #f "<pre class=\"~a\">\n~a</pre>" l bc))
      (_ "")))

  (string-join (map render ast) "\n"))

(define (org->html str)
  "Convert a Org-mode string into HTML."
  (org-ast->html (org->ast str)))
