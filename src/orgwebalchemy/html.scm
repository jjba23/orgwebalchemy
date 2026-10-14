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
                             image-classes
                             figure-classes
                             figure-caption-classes
                             heading-id
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
                             table-cell-classes
                             strike-classes
                             superscript-classes
                             subscript-classes
                             keyword-classes
                             fixed-width-classes
                             verse-block-classes
                             center-block-classes
                             checkbox-classes))

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

(define (add-sxml-id node id)
  "Return the SXML @var{node} with the attribute @var{id} added to it."
  (let ((tag (car node))
        (rest (cdr node)))
    (if (and (pair? rest)
             (pair? (car rest))
             (eq? (car (car rest))
                  '@))
        `(,tag (@ (id ,id)
                  ,@(cdr (car rest)))
          ,@(cdr rest))
        `(,tag ,@(make-sxml-attrs '()
                                  (list (list 'id id)))
          ,@rest))))

(define (heading-id text)
  "Return the `id' attributes for a heading whose title is @var{text}.
The id is exactly the anchor an Org `*Heading' link points at, so a heading and
the links to it agree by construction.  A title with nothing anchor-worthy in
it (an empty one, or one made only of punctuation) gets no id at all."
  (let ((id (org-anchor (or text ""))))
    (if (string-null? id)
        '()
        `((id ,id)))))

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

;; @var{checkbox} is the value @code{split-checkbox} returns: @code{#f}, or the
;; state of the box and the item text with the box removed.
(define (render-checkbox checkbox)
  "Return a list holding an SXML checkbox input for @var{checkbox}, or '().
@var{checkbox} is the value produced by @code{split-checkbox}."
  (if (not checkbox)
      '()
      (list `(input ,@(make-sxml-attrs (checkbox-classes)
                                       (append '((type "checkbox")
                                                 (disabled "disabled"))
                                               (if (car checkbox)
                                                   '((checked "checked"))
                                                   '())))))))

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

      ;; +strikethrough+
      (('inline-strike-content text)
       `(del ,@(make-sxml-attrs (strike-classes))
             ,@(inline-org->sxml (inline-org->ast text))))

      ;; x^{superscript}
      (('inline-superscript-content text)
       `(sup ,@(make-sxml-attrs (superscript-classes))
             ,@(inline-org->sxml (inline-org->ast text))))

      ;; x_{subscript}
      (('inline-subscript-content text)
       `(sub ,@(make-sxml-attrs (subscript-classes))
             ,@(inline-org->sxml (inline-org->ast text))))

      ;; An escaped markup character, emitted literally.
      (('inline-escape-char text)
       text)

      ;; Forced line break.
      (('inline-line-break _)
       '(br))

      ;; [fn:label]
      (('inline-footnote-ref ('footnote-label label))
       `(sup ,@(make-sxml-attrs '("footnote-ref"))
             (a (@ (href ,(format #f "#fn-~a" label))
                   (id ,(format #f "fnref-~a" label)))
                ,label)))

      ;; [fn:label: definition]
      (('inline-footnote-def ('footnote-label label)
        ('footnote-content definition))
       `(span ,@(make-sxml-attrs '("footnote-def")
                                 `((id ,(format #f "fn-~a" label))))
              (sup ,@(make-sxml-attrs '("footnote-label"))
                   ,label)
              ,definition))

      ;; [fn::inline definition]
      (('inline-footnote-inline ('footnote-content definition))
       `(sup ,@(make-sxml-attrs '("footnote-inline"))
             ,definition))

      ;; [[target][description]] -- an image link becomes an `img' whose alt is
      ;; the description, every other link an `a' whose href comes from the
      ;; shared link classifier (so `file:' links, `#custom-id' links and
      ;; `*Heading' links all point somewhere real).
      (('inline-link-with-desc ('link-target-content url)
        ('link-description-content description))
       (if (org-image-link? url)
           `(img ,@(make-sxml-attrs (image-classes)
                                    `((src ,(org-link-href url))
                                      (alt ,description))))
           `(a ,@(make-sxml-attrs (link-classes)
                                  `((href ,(org-link-href url))))
               ,@(inline-org->sxml (inline-org->ast description)))))

      ;; [[target]] -- the link text is what Org would show: the path, the id,
      ;; or the full URL.
      (('inline-link-no-desc ('link-target-content url))
       (if (org-image-link? url)
           `(img ,@(make-sxml-attrs (image-classes)
                                    `((src ,(org-link-href url))
                                      (alt ,(org-link-display url)))))
           `(a ,@(make-sxml-attrs (link-classes)
                                  `((href ,(org-link-href url))))
               ,(org-link-display url))))

      ;; URL
      (('inline-raw-url ('url-scheme scheme) url)
       `(a ,@(make-sxml-attrs (link-classes)
                              `((href ,(format #f "~a~a" scheme url))))
           ,(format #f "~a~a" scheme url)))

      ;; Plain text
      (('inline-text text)
       text)

      ;; Raw string fallback
      ((? string? text)
       text)

      (_ #f)))

  ;; Whatever shape the parse came in, we map over a list of nodes.
  (filter-map (lambda (node)
                (let ((res (render node)))
                  (if (equal? res "") #f res)))
              (inline-nodes ast)))

(define (inline-org->html ast)
  "Convert an inline Org AST into an HTML string."
  (sxml->html-string (inline-org->sxml ast)))

;;; List Rendering Helpers

(define (render-nested-lists sub-nodes)
  "Render the @var{sub-nodes} of a list item, expanding nested lists of any
kind into list elements of their own."
  (append-map (lambda (node)
                (match node
                  (('nested-list items ...)
                   (render-list items))
                  (_ (org-ast->sxml node)))) sub-nodes))

(define (render-list-item item classes)
  "Render one list @var{item}, unordered or ordered, as an `li'.
@var{classes} is the parameter that holds the classes for this kind of item."
  (match item
    (((or 'unordered-item
          'ordered-item)
      ('line-content text) sub ...)
     (let* ((checkbox (split-checkbox text))
            (body (if checkbox
                      (cdr checkbox) text)))
       `(li ,@(make-sxml-attrs (classes))
            ,@(render-checkbox checkbox)
            ,@(inline-org->sxml (inline-org->ast body))
            ,@(render-nested-lists sub))))
    (_ #f)))

(define (render-desc-item item)
  (match item
    (('desc-item ('desc-key key)
      ('line-content text) sub ...)
     `((dt ,@(make-sxml-attrs (description-term-classes))
           ,@(inline-org->sxml (inline-org->ast key)))
       (dd ,@(make-sxml-attrs (description-detail-classes))
           ,@(inline-org->sxml (inline-org->ast text))
           ,@(render-nested-lists sub))))
    (_ '())))

(define (render-list-group kind items)
  "Render one run of same-kind list @var{items} as a single list element."
  (cond
    ((eq? kind
          'unordered)
     `(ul ,@(make-sxml-attrs (unordered-list-classes))
          ,@(filter-map (lambda (item)
                          (render-list-item item unordered-list-item-classes))
                        items)))
    ((eq? kind
          'ordered)
     `(ol ,@(make-sxml-attrs (ordered-list-classes))
          ,@(filter-map (lambda (item)
                          (render-list-item item ordered-list-item-classes))
                        items)))
    ((eq? kind
          'description)
     `(dl ,@(make-sxml-attrs (description-list-classes))
          ,@(append-map render-desc-item items)))
    (else #f)))

(define (render-list items)
  "Render a list's @var{items}, splitting them into runs of the same kind."
  (filter-map (lambda (group)
                (render-list-group (car group)
                                   (cdr group)))
              (group-list-items items)))

;;; Block Org AST -> SXML Transformation

(define (render-heading stars children)
  "Render a heading with @var{stars} and @var{children} to SXML.
The heading level is the number of stars, which is the number of the `h'
element as well; the TODO keyword, the priority, the title and the tags are the
parts @code{heading-parts} reads out of @var{children}."
  (let* ((parts (heading-parts children))
         (todo (list-ref parts 0))
         (priority (list-ref parts 1))
         (tags (list-ref parts 2))
         (text (or (list-ref parts 3) ""))
         (level (string-length stars))
         (tag (string->symbol (format #f "h~a" level)))
         (classes (append ((heading-classes)
                           level)
                          (if todo
                              (list "org-todo"
                                    (format #f "org-todo-~a"
                                            (string-downcase todo)))
                              '()))))
    `(,tag ,@(make-sxml-attrs classes
                              (heading-id text))
      ,@(if todo
            (list `(span ,@(make-sxml-attrs '("org-todo"))
                         ,todo))
            '())
      ,@(if priority
            (list `(span ,@(make-sxml-attrs '("org-priority"))
                         ,(format #f "[#~a]" priority)))
            '())
      ,@(inline-org->sxml (inline-org->ast text))
      ,@(if (null? tags)
            '()
            (list `(span ,@(make-sxml-attrs '("org-tags"))
                         ,@(map (lambda (name)
                                  `(span ,@(make-sxml-attrs '("org-tag"))
                                         ,name)) tags)))))))

(define (org-ast->sxml ast)
  "Convert a document Org AST into a list of SXML block nodes."
  (define (render node)
    (match node
      ;; Headings
      (((quote heading) (? string? stars) . children) (render-heading stars
                                                       children))

      ;; Horizontal Separator
      (('separator sep-content)
       `(hr ,@(make-sxml-attrs (separator-classes))))

      ;; Lists, which may mix item kinds and nest lists of a different kind.
      (('org-list items ...)
       (render-list items))
      (('nested-list items ...)
       (render-list items))

      ;; Paragraph
      (('paragraph ('line-content text))
       `(p ,@(make-sxml-attrs (paragraph-classes))
           ,@(inline-org->sxml (inline-org->ast text))))

      ;; Figure: an image that the `#+CAPTION:' / `#+NAME:' keywords above it
      ;; describe.  The name, when there is one, is the element's id, exactly
      ;; as Org does it, so a `[[fig:arch]]' link -- which the link classifier
      ;; already turns into the fragment `#fig:arch' -- lands on the figure.
      (('figure _ ...)
       (match (figure-parts node)
         ((name caption target description)
          `(figure ,@(make-sxml-attrs (figure-classes)
                                      (if name
                                          `((id ,name))
                                          '()))
                   (img ,@(make-sxml-attrs (image-classes)
                                           `((src ,(org-link-href target))
                                             (alt ,(or description caption
                                                       (org-link-display
                                                        target))))))
                   ,@(if caption
                         (list `(figcaption ,@(make-sxml-attrs (figure-caption-classes))
                                            ,caption))
                         '())))))

      ;; A named element: `#+NAME:' above an element is its id, exactly as it is
      ;; for a figure, so a `[[tbl:data]]' link -- which the link classifier
      ;; already turns into the fragment `#tbl:data' -- lands on the element
      ;; itself.  Elements that are not their own HTML element, a comment being
      ;; the one that matters, get an anchor of their own to carry the id.
      (('named _ ...)
       (match (named-parts node)
         ((name inner)
          (let* ((nodes (inline-nodes (render inner)))
                 (tag (and (pair? nodes)
                           (pair? (car nodes))
                           (car (car nodes)))))
            (if (and tag
                     (not (eq? tag
                               'raw-html)))
                (cons (add-sxml-id (car nodes) name)
                      (cdr nodes))
                (cons `(a ,@(make-sxml-attrs '()
                                             (list (list 'id name)))) nodes))))))

      ;; In-buffer keyword / metadata, e.g. #+TITLE: Hello
      (('keyword ('keyword-key key)
        ('keyword-value value))
       `(div ,@(make-sxml-attrs (keyword-classes))
             ,(format #f "~a: ~a" key value)))

      ;; A single commented line.
      (('comment-line ('comment-content text))
       `(raw-html ,(format #f "<!-- ~a -->" text)))

      ;; A commented block.
      (('comment-block ('comment-block-content text))
       `(raw-html ,(format #f "<!-- ~a-->" text)))

      ;; Fixed-width line.
      (('fixed-width ('fixed-width-content text))
       `(pre ,@(make-sxml-attrs (fixed-width-classes))
             ,(string-append text "\n")))

      ;; Verse block: literal lines joined by forced line breaks.
      (('verse-block ('verse-content text))
       (let ((lines (string-split (string-trim-right text
                                                     (lambda (c)
                                                       (char=? c #\newline)))
                                  #\newline)))
         `(p ,@(make-sxml-attrs (verse-block-classes))
             ,@(let loop
                 ((ls lines))
                 (cond
                   ((null? ls)
                    '())
                   ((null? (cdr ls))
                    (list (car ls)))
                   (else (cons (car ls)
                               (cons '(br)
                                     (loop (cdr ls))))))))))

      ;; Center block: contents are ordinary Org, centred in HTML.
      (('center-block ('center-content text))
       `(div ,@(make-sxml-attrs (center-block-classes))
             (p ,@(inline-org->sxml (inline-org->ast (string-trim-right text
                                                                        (lambda 
                                                                                (c)
                                                                          (char=?
                                                                           c
                                                                           #\newline))))))))

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

  ;; The document's own `#+NAME:' targets are in scope while it renders, so a
  ;; bare `[[name]]' link is parsed as the cross-reference it is.
  (parameterize ((element-names (document-names ast)))
    ;; A single node may render to several sibling nodes (a mixed list renders
    ;; to one or more list elements, a description item to a <dt>/<dd> pair),
    ;; so the results are spliced rather than collected.
    (append-map (lambda (node)
                  (inline-nodes (render node)))
                (inline-nodes ast))))

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

(define image-classes
  (make-parameter '()))

(define figure-classes
  (make-parameter '()))

(define figure-caption-classes
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

(define strike-classes
  (make-parameter '()))

(define superscript-classes
  (make-parameter '()))

(define subscript-classes
  (make-parameter '()))

(define keyword-classes
  (make-parameter '()))

(define fixed-width-classes
  (make-parameter '()))

(define verse-block-classes
  (make-parameter '()))

(define center-block-classes
  (make-parameter '()))

(define checkbox-classes
  (make-parameter '()))
