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
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (orgwebalchemy peg)
  #:export (inline-org->sxml inline-org->html
                             org->sxml
                             org->html
                             org-ast->sxml
                             org-ast->html
                             sxml->html-string
                             link-classes
                             code-classes
                             code-block-classes
                             italic-classes
                             bold-classes
                             hr-classes))

;;; Configuration Parameters

(define link-classes
  (make-parameter '()))

(define code-classes
  (make-parameter '()))

(define code-block-classes
  (make-parameter '()))

(define italic-classes
  (make-parameter '()))

(define bold-classes
  (make-parameter '()))

(define hr-classes
  (make-parameter '()))

;;; Helper Utilities

(define (make-attrs classes extra-attrs)
  "Construct an SXML (@ ...) attribute node if classes or extra attributes exist."
  (let ((cls-str (string-join classes " ")))
    (cond
      ((and (not (string-null? cls-str))
            (not (null? extra-attrs)))
       `((@ (class ,cls-str)
            ,@extra-attrs)))
      ((not (string-null? cls-str))
       `((@ (class ,cls-str))))
      ((not (null? extra-attrs))
       `((@ ,@extra-attrs)))
      (else '()))))

(define (sxml->html-string sxml)
  "Render an SXML tree or list of SXML nodes to an HTML string."
  (call-with-output-string (lambda (port)
                             (if (and (pair? sxml)
                                      (symbol? (car sxml)))
                                 (sxml->xml sxml port)
                                 (sxml->xml `(*TOP* ,@sxml) port)))))

;;; Inline Org -> SXML Transformation

(define (inline-org->sxml ast)
  "Convert an inline Org AST into a list of SXML nodes."
  (define (render node)
    (match node
      ;; Escaped markup
      (('inline-escape text)
       text)

      ;; ~code~
      (('inline-code-content text)
       `(code ,@(make-attrs (code-classes)
                            '())
              ,text))

      ;; =verbatim=
      (('inline-verbatim-content text)
       `(code ,@(make-attrs (code-classes)
                            '())
              ,text))

      ;; /italic/
      (('inline-italic-content text)
       `(em ,@(make-attrs (italic-classes)
                          '())
            ,@(inline-org->sxml (inline-org->ast text))))

      ;; *bold*
      (('inline-bold-content text)
       `(strong ,@(make-attrs (bold-classes)
                              '())
                ,@(inline-org->sxml (inline-org->ast text))))

      ;; _underline_
      (('inline-underline-content text)
       `(u ,@(inline-org->sxml (inline-org->ast text))))

      ;; [[URL][description]]
      (('inline-link-with-desc ('link-target-content url)
        ('link-description-content description))
       `(a ,@(make-attrs (link-classes)
                         `((href ,url)))
           ,@(inline-org->sxml (inline-org->ast description))))

      ;; [[URL]]
      (('inline-link-no-desc ('link-target-content url))
       `(a ,@(make-attrs (link-classes)
                         `((href ,url)))
           ,url))

      ;; Plain text
      (('inline-text text)
       text)

      ;; Raw string fallback
      ((? string? text)
       text)

      (_ #f)))

  (filter-map (lambda (node)
                (let ((res (render node)))
                  (if (equal? res "") #f res))) ast))

(define (inline-org->html ast)
  "Convert an inline Org AST into an HTML string."
  (sxml->html-string (inline-org->sxml ast)))

;;; Block Org AST -> SXML Transformation

(define (org-ast->sxml ast)
  "Convert a document Org AST into a list of SXML block nodes."
  (define (render node)
    (match node
      (('heading ('stars s)
        ('line-content text))
       (let ((lvl (string-length s)))
         (let ((tag (string->symbol (format #f "h~a" lvl))))
           `(,tag ,@(inline-org->sxml (inline-org->ast text))))))

      (('description-list items ...)
       `(dl ,@(append-map (lambda (li)
                            (match li
                              (('unordered-item ('desc-key key)
                                ('line-content text))
                               `((dt ,@(inline-org->sxml (inline-org->ast key)))
                                 (dd ,@(inline-org->sxml (inline-org->ast text)))))
                              (_ '()))) items)))

      (('unordered-list items ...)
       `(ul ,@(filter-map (lambda (li)
                            (match li
                              (('unordered-item ('line-content text))
                               `(li ,@(inline-org->sxml (inline-org->ast text))))
                              (_ #f))) items)))

      (('ordered-list items ...)
       `(ol ,@(filter-map (lambda (li)
                            (match li
                              (('ordered-item ('line-content text))
                               `(li ,@(inline-org->sxml (inline-org->ast text))))
                              (_ #f))) items)))

      (('paragraph ('line-content text))
       `(p ,@(inline-org->sxml (inline-org->ast text))))

      (('src-block ('lang l)
        ('block-content bc))
       (let ((lang-class (if (and (string? l)
                                  (not (string-null? l)))
                             (list l)
                             '())))
         `(pre ,@(make-attrs (append lang-class
                                     (code-block-classes))
                             '())
               ,bc)))

      (('horizontal-rule)
       `(hr ,@(make-attrs (hr-classes)
                          '())))

      (_ #f)))

  (filter-map render ast))

(define (org-ast->html ast)
  "Convert a document Org AST into an HTML string."
  (sxml->html-string (org-ast->sxml ast)))

(define (org->sxml str)
  "Convert an Org-mode string into SXML."
  (org-ast->sxml (org->ast str)))

(define (org->html str)
  "Convert an Org-mode string into an HTML string."
  (sxml->html-string (org->sxml str)))
