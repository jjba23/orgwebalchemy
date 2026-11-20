;; orgwebalchemy
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The AST as JSON: the tree the renderers walk, written down as data another
;; language can read.  The shape is the contract and there are only three
;; rules:
;;
;;   - a document is always a JSON array of nodes, so one element and ten
;;     elements arrive the same way;
;;   - a node is always `{"type": "<tag>", "children": [...]}', with the tag
;;     spelling the node of the vocabulary, so no node needs a rule of its own;
;;   - everything else is a JSON string.
;;
;; Prose is resolved first: a `line-content', a `desc-key', a `table-cell' and
;; the fields that carry markup inside markup become the inline nodes the
;; renderers see, because a caller that has to reimplement the inline grammar
;; has not been given the tree.
;;
;; Guile's standard library has no JSON writer and this library has no runtime
;; dependency beyond Guile, so the writer is here: three rules is little enough
;; to write down, and nothing else is needed to serialise the vocabulary.

(define-module (orgwebalchemy json)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (orgwebalchemy peg)
  #:export (org->json org-ast->json inline-org->json json-string))

;; =====================================================================
;; Strings
;; =====================================================================

(define json-short-escapes
  ;; The five whitespace characters JSON has a name for.  A character with a
  ;; name reads better in a diff than the same character numbered.
  '((#\newline . "\\n")
    (#\return . "\\r")
    (#\tab . "\\t")
    (#\backspace . "\\b")
    (#\page . "\\f")))

(define (json-string str)
  "Return @var{str} as a JSON string literal, quotes included.
A quote, a backslash and a character below U+0020 are escaped as JSON requires;
everything else, non-ASCII included, is written as the UTF-8 text it is."
  (call-with-output-string (lambda (port)
                             (write-char #\" port)
                             (string-for-each (lambda (c)
                                                (let ((escape (assv c
                                                                    json-short-escapes)))
                                                  (cond
                                                    ((char=? c
                                                             #\")
                                                     (display "\\\""
                                                              port))
                                                    ((char=? c
                                                             #\\)
                                                     (display "\\\\"
                                                              port))
                                                    (escape (display (cdr escape)
                                                                     port))
                                                    ((char<? c
                                                              #\space)
                                                     (format port "\\u~4,'0x"
                                                             (char->integer c)))
                                                    (else (write-char c port)))))
                                              str)
                             (write-char #\" port))))

;; =====================================================================
;; The Tree
;; =====================================================================

(define prose-fields
  ;; The nodes whose one child is Org text rather than plain text: the fields
  ;; the renderers hand to the inline parser before they write anything.  A
  ;; field that is not here is literal -- a source block's contents, a
  ;; keyword's value -- and reaches JSON exactly as it was written.
  '(line-content
    desc-key
    table-cell
    inline-bold-content
    inline-italic-content
    inline-underline-content
    inline-strike-content
    inline-superscript-content
    inline-subscript-content
    link-description-content
    footnote-content))

(define (resolve-prose node)
  "Return @var{node} with every prose field replaced by the nodes it parses to.
The inline parser is the renderers' own, so the tree this gives a foreign
language is the tree this library renders from."
  (cond
    ((string? node)
     node)
    ((and (pair? node)
          (symbol? (car node)))
     (if (and (memq (car node)
                    prose-fields)
              (pair? (cdr node))
              (string? (cadr node)))
         (cons (car node)
               (map resolve-prose
                    (inline-nodes (inline-org->ast (cadr node)))))
         (cons (car node)
               (map resolve-prose (cdr node)))))
    ((pair? node)
     (map resolve-prose node))
    (else node)))

(define (write-array nodes port)
  "Write the list @var{nodes} to @var{port} as a JSON array."
  (display "[" port)
  (let loop
    ((nodes nodes)
     (rest? #f))
    (when (pair? nodes)
      (when rest?
        (display "," port))
      (write-value (car nodes) port)
      (loop (cdr nodes) #t)))
  (display "]" port))

(define (write-value node port)
  "Write one value of the tree @var{node} to @var{port}.
A string is a JSON string and a node is an object of a type and its children,
which is every value an AST holds; anything else is an error rather than a
`null' nobody can explain later.  A node with no children arrives as the bare
tag symbol, because an empty match collapses to it in the tree, so a symbol is
a node with an empty child list rather than a gap."
  (cond
    ((string? node)
     (display (json-string node) port))
    ((and (pair? node)
          (symbol? (car node)))
     (write-object (car node)
                   (cdr node)
                   port))
    ((symbol? node)
     (write-object node
                   '()
                   port))
    ((list? node)
     (write-array node port))
    (else (error "orgwebalchemy json: a string or a node was expected, got"
                 node))))

(define (write-object type children port)
  "Write the node @var{type} with @var{children} to @var{port} as an object."
  (display "{\"type\":" port)
  (display (json-string (symbol->string type)) port)
  (display ",\"children\":" port)
  (write-array children port)
  (display "}" port))

;; =====================================================================
;; Entry Points
;; =====================================================================

(define (org-ast->json ast)
  "Return the document AST @var{ast} as a JSON array of nodes.
The document's own `#+NAME:' targets are in scope while it is written, so a
bare `[[name]]' link is serialised as the fragment the renderers give it."
  (parameterize ((element-names (document-names ast)))
    (call-with-output-string (lambda (port)
                               (write-array (map resolve-prose (inline-nodes
                                                                ast))
                                            port)))))

(define (org->json str)
  "Parse the Org document @var{str} and return its tree as JSON text."
  (org-ast->json (org->ast str)))

(define (inline-org->json str)
  "Parse the inline Org markup @var{str} and return its nodes as JSON text."
  (call-with-output-string (lambda (port)
                             (write-array (map resolve-prose
                                               (inline-nodes (inline-org->ast
                                                              str)))
                                          port))))
