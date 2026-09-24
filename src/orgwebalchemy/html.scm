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
  #:declarative? #t
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
                             make-sxml-attrs
                             code-block-classes
                             quote-block-classes
                             example-block-classes
                             italic-classes
                             bold-classes
                             heading-classes
                             underline-classes
                             paragraph-classes
                             separator-classes
                             description-list-classes
                             description-term-classes
                             description-detail-classes
                             unordered-list-classes
                             unordered-list-item-classes
                             ordered-list-classes
                             ordered-list-item-classes
                             table-classes
                             table-header-row-classes
                             table-header-cell-classes
                             table-row-classes
                             table-cell-classes))

;; =======================================================================
;; Helpers & Utilities
;; =======================================================================

(define* (make-sxml-attrs classes
                          #:optional (extra-attrs '()))
  "Construct an SXML attribute node if classes or extra attributes exist."
  (let ((cls-str (string-join (map (lambda (c)
                                     (format #f "~a" c)) classes) " ")))
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
  "Render an SXML tree or list of SXML nodes to an HTML string,
preserving raw HTML nodes."
  (define (render-node node port)
    (match node
      ;; Raw HTML (custom tag) is emitted without escaping.
      (('raw-html html)
       (display html port))

      ;; Nested list of SXML nodes.
      ((? pair?)
       (if (and (pair? node)
                (symbol? (car node)))
           (sxml->xml node port)
           (for-each (lambda (child)
                       (render-node child port)) node)))

      ;; Strings are emitted literally here.
      ;; SXML handles escaping when they occur inside ordinary elements.
      (_ (display node port))))

  (call-with-output-string (lambda (port)
                             (cond
                               ;; Strip *TOP* wrapper if it exists
                               ((and (pair? sxml)
                                     (eq? (car sxml)
                                          '*TOP*))
                                (for-each (lambda (child)
                                            (render-node child port))
                                          (cdr sxml)))

                               ;; It's a single valid SXML node
                               ((and (pair? sxml)
                                     (symbol? (car sxml)))
                                (render-node sxml port))

                               ;; It's a list of nodes -> render each
                               (else (for-each (lambda (node)
                                                 (render-node node port)) sxml))))))

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
       `(code ,@(make-sxml-attrs (code-classes)
                                 '())
              ,text))

      ;; =verbatim=
      (('inline-verbatim-content text)
       `(code ,@(make-sxml-attrs (code-classes)
                                 '())
              ,text))

      ;; /italic/
      (('inline-italic-content text)
       `(em ,@(make-sxml-attrs (italic-classes)
                               '())
            ,@(inline-org->sxml (inline-org->ast text))))

      ;; *bold*
      (('inline-bold-content text)
       `(strong ,@(make-sxml-attrs (bold-classes)
                                   '())
                ,@(inline-org->sxml (inline-org->ast text))))

      ;; _underline_
      (('inline-underline-content text)
       `(u ,@(make-sxml-attrs (underline-classes))
           ,@(inline-org->sxml (inline-org->ast text))))

      ;; [[URL][description]]
      (('inline-link-with-desc ('link-target-content url)
        ('link-description-content description))
       `(a ,@(make-sxml-attrs (link-classes)
                              `((href ,url)))
           ,@(inline-org->sxml (inline-org->ast description))))

      ;; [[URL]]
      (('inline-link-no-desc ('link-target-content url))
       `(a ,@(make-sxml-attrs (link-classes)
                              `((href ,url)))
           ,url))

      ;; Plain text
      (('inline-text text)
       text)

      ;; Raw string fallback
      ((? string? text)
       text)

      (_ #f)))

  ;; Ensure we are mapping over a list of nodes, even if PEG returned a single node
  (let ((nodes (cond
                 ((string? ast)
                  (list ast))
                 ((and (pair? ast)
                       (symbol? (car ast)))
                  (list ast))
                 (else ast))))
    (filter-map (lambda (node)
                  (let ((res (render node)))
                    (if (equal? res "") #f res))) nodes)))

(define (inline-org->html ast)
  "Convert an inline Org AST into an HTML string."
  (sxml->html-string (inline-org->sxml ast)))

;;; Block Org AST -> SXML Transformation

(define (org-ast->sxml ast)
  "Convert a document Org AST into a list of SXML block nodes."
  (define (render node)
    (match node
      
      ;; Headings
      (('heading ('stars s)
        ('line-content text))
       (let* ((lvl (string-length s))
              (tag (string->symbol (format #f "h~a" lvl)))
              (classes ((heading-classes)
                        lvl)))
         `(,tag ,@(make-sxml-attrs classes)
           ,@(inline-org->sxml (inline-org->ast text)))))

      ;; Horizontal Separator
      (('separator sep-content)
       `(hr ,@(make-sxml-attrs (separator-classes))))

      ;; Description lists
      (((or 'description-list
            'nested-description-list)
        items ...)
       `(dl ,@(make-sxml-attrs (description-list-classes))
            ,@(append-map (lambda (li)
                            (match li
                              (((or 'nested-desc-item
                                    'desc-item)
                                ('desc-key key)
                                ('line-content text) sub-nodes ...)
                               `((dt ,@(make-sxml-attrs (description-term-classes))
                                     ,@(inline-org->sxml (inline-org->ast key)))
                                 (dd ,@(make-sxml-attrs (description-detail-classes))
                                     ,@(inline-org->sxml (inline-org->ast text))
                                     ,@(org-ast->sxml sub-nodes))))
                              (_ '()))) items)))

      ;; Unordered lists
      (((or 'unordered-list
            'nested-unordered-list)
        items ...)
       `(ul ,@(make-sxml-attrs (unordered-list-classes))
            ,@(filter-map (lambda (li)
                            (match li
                              (((or 'unordered-item
                                    'nested-unordered-item)
                                ('line-content text) sub-nodes ...)
                               `(li ,@(make-sxml-attrs (unordered-list-item-classes))
                                    ,@(inline-org->sxml (inline-org->ast text))
                                    ,@(org-ast->sxml sub-nodes)))
                              (_ #f))) items)))

      ;; Ordered lists
      (((or 'ordered-list
            'nested-ordered-list)
        items ...)
       `(ol ,@(make-sxml-attrs (ordered-list-classes))
            ,@(filter-map (lambda (li)
                            (match li
                              (((or 'ordered-item
                                    'nested-ordered-item)
                                ('line-content text) sub-nodes ...)
                               `(li ,@(make-sxml-attrs (ordered-list-item-classes))
                                    ,@(inline-org->sxml (inline-org->ast text))
                                    ,@(org-ast->sxml sub-nodes)))
                              (_ #f))) items)))

      ;; Paragraph
      (('paragraph ('line-content text))
       `(p ,@(make-sxml-attrs (paragraph-classes))
           ,@(inline-org->sxml (inline-org->ast text))))

      ;; Source block
      (('src-block ('lang l)
        ('src-content bc))
       (let ((lang-class (if (and (string? l)
                                  (not (string-null? l)))
                             (list l)
                             '())))
         `(pre ,@(make-sxml-attrs (append lang-class
                                          (code-block-classes)))
               ,bc)))

      ;; Quote block
      ;;
      ;; Quote text is regular Org content, so inline markup
      ;; should still be interpreted.
      (('quote-block ('quote-content text))
       `(blockquote ,@(make-sxml-attrs (quote-block-classes))
                    ,@(inline-org->sxml (inline-org->ast text))))

      ;; Example block
      ;;
      ;; Example contents are literal. Do not parse them as Org.
      (('example-block ('example-content text))
       `(pre ,@(make-sxml-attrs (example-block-classes))
             ,text))

      ;; HTML export block
      ;;
      ;; The contents are already HTML. Mark them as raw with custom tag so that
      ;; they are not escaped or interpreted as Org markup.
      (('export-html-block ('export-html-content html))
       `(raw-html ,html))

      ;; Tables
      (('table items ...)
       `(table ,@(make-sxml-attrs (table-classes))
               ,@(org-ast->sxml items)))

      (('table-header-row items ...)
       `(thead (tr ,@(make-sxml-attrs (table-header-row-classes))
                   ,@(map (lambda (cell)
                            (match cell
                              (('table-cell text)
                               `(th ,@(make-sxml-attrs (table-header-cell-classes))
                                    ,@(inline-org->sxml (inline-org->ast text))))
                              (_ #f))) items))))

      (('table-row items ...)
       `(tbody (tr ,@(make-sxml-attrs (table-row-classes))
                   ,@(map (lambda (cell)
                            (match cell
                              (('table-cell text)
                               `(td ,@(make-sxml-attrs (table-cell-classes))
                                    ,@(inline-org->sxml (inline-org->ast text))))
                              (_ #f))) items))))

      ;; fallback when no match found
      (_ #f)))

  (let ((nodes (cond
                 ((string? ast)
                  (list ast))
                 ((and (pair? ast)
                       (symbol? (car ast)))
                  (list ast))
                 (else ast))))
    (filter-map (lambda (node)
                  (let ((res (render node)))
                    (if (equal? res "") #f res))) nodes)))

(define (org-ast->html ast)
  "Convert a document Org AST into an HTML string."
  (sxml->html-string (org-ast->sxml ast)))

(define (org->sxml str)
  "Convert an Org-mode string into SXML."
  (org-ast->sxml (org->ast str)))

(define (org->html str)
  "Convert an Org-mode string into an HTML string."
  (sxml->html-string (org->sxml str)))

;; =============================================
;; Configuration Parameters
;; =============================================

(define link-classes
  (make-parameter '()))

(define code-classes
  (make-parameter '()))

(define code-block-classes
  (make-parameter '()))

(define quote-block-classes
  (make-parameter '()))

(define example-block-classes
  (make-parameter '()))

(define italic-classes
  (make-parameter '()))

(define bold-classes
  (make-parameter '()))

(define separator-classes
  (make-parameter '()))

(define paragraph-classes
  (make-parameter '()))

(define underline-classes
  (make-parameter '()))

(define table-classes
  (make-parameter '()))

(define table-header-row-classes
  (make-parameter '()))

(define table-header-cell-classes
  (make-parameter '()))

(define table-row-classes
  (make-parameter '()))

(define table-cell-classes
  (make-parameter '()))

(define heading-classes
  ;; heading-classes is a more complex item, as it handles multiple header levels
  ;; and can be customized as so:
  ;;
  ;; (heading-classes
  ;; (lambda (level)
  ;; (case level
  ;; ((1) '("text-4xl" "font-bold" "mb-4"))
  ;; ((2) '("text-2xl" "font-semibold" "mb-3"))
  ;; ((3) '("text-xl" "font-medium" "mb-2"))
  ;; (else '("text-base" "font-normal")))))
  ;;
  ;; or:
  ;; (heading-classes
  ;; (lambda (level)
  ;; (list (format #f "org-heading-lvl-~a" level) "header-element")))
  ;;
  ;; defaults to a procedure taking the heading level (integer)
  ;; and returning an empty list of classes.
  (make-parameter (lambda _
                    '())))

(define description-list-classes
  (make-parameter '()))

(define description-term-classes
  (make-parameter '()))

(define description-detail-classes
  (make-parameter '()))

(define unordered-list-classes
  (make-parameter '()))

(define unordered-list-item-classes
  (make-parameter '()))

(define ordered-list-classes
  (make-parameter '()))

(define ordered-list-item-classes
  (make-parameter '()))
