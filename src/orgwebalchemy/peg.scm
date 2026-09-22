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

(define-module (orgwebalchemy peg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (org->ast inline-org->ast))

;; =====================================================================
;; Org-mode PEG (Parsing Expression Grammar)
;; =====================================================================
;;
;; some tips for `define-peg-pattern'
;; 'none' means these match in the string but won't appear in the final AST.
;; 'body' means the node itself is omitted, but its matched children bubble up.
;; 'all' means both the tag and the matched string/children are kept in the AST.
;;

;; =====================================================================
;; Character & Whitespace Primitives
;; =====================================================================

(define-peg-pattern nl none
                    (or "\r\n" "\n"))
(define-peg-pattern ws none
                    (or " " "\t"))
(define-peg-pattern eof none
                    (not-followed-by peg-any))
(define-peg-pattern digit none
                    (range #\0 #\9))

;; =====================================================================
;; Core Document Structure
;; =====================================================================

(define-peg-pattern org-doc body
                    (* element))

(define-peg-pattern element body
                    (or empty-line
                        heading
                        src-block
                        description-list
                        unordered-list
                        ordered-list
                        paragraph))

(define-peg-pattern empty-line none
                    (and (* ws) nl))

;; =====================================================================
;; Block Level Elements
;; =====================================================================

;; Headings (*, **, ***)
(define-peg-pattern heading all
                    (and stars
                         (+ ws) line-content
                         (or nl eof)))
(define-peg-pattern stars all
                    (+ "*"))

;; =====================================================================
;; List Items & Recursive List Containers
;; =====================================================================

(define-peg-pattern list-bullet none
                    (or "-" "+"))

(define-peg-pattern list-num none
                    (and (+ digit)
                         (or "." ")")))

(define-peg-pattern desc-sep none
                    (and (+ ws) "::"
                         (or (+ ws)
                             (followed-by nl)
                             (followed-by eof))))

(define-peg-pattern desc-key all
                    (+ (and (not-followed-by (or nl desc-sep eof)) peg-any)))

(define-peg-pattern nested-indent none
                    (and "  "
                         (* ws)))

;; Nested list wrapper
(define-peg-pattern sub-list body
                    (or description-list unordered-list ordered-list))

;; --- Description Lists ---

(define-peg-pattern desc-item all
                    (and (* ws)
                         (or list-bullet list-num)
                         (+ ws)
                         desc-key
                         desc-sep
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern nested-desc-item all
                    (and nested-indent
                         (or list-bullet list-num)
                         (+ ws)
                         desc-key
                         desc-sep
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern description-list all
                    (+ (or nested-desc-item desc-item)))

;; --- Unordered Lists ---

(define-peg-pattern unordered-item all
                    (and (* ws)
                         list-bullet
                         (+ ws)
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern nested-unordered-item all
                    (and nested-indent
                         list-bullet
                         (+ ws)
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern unordered-list all
                    (+ (or nested-unordered-item unordered-item)))

;; --- Ordered Lists ---

(define-peg-pattern ordered-item all
                    (and (* ws)
                         list-num
                         (+ ws)
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern nested-ordered-item all
                    (and nested-indent
                         list-num
                         (+ ws)
                         line-content
                         (or nl eof)
                         (* sub-list)))

(define-peg-pattern ordered-list all
                    (+ (or nested-ordered-item ordered-item)))

;; =====================================================================
;; Source Blocks (#+BEGIN_SRC ... #+END_SRC)
;; =====================================================================

(define-peg-pattern begin-src none
                    (or "#+BEGIN_SRC" "#+begin_src"))
(define-peg-pattern end-src none
                    (or "#+END_SRC" "#+end_src"))

(define-peg-pattern lang all
                    (* (and (not-followed-by nl) peg-any)))

(define-peg-pattern src-block all
                    (and begin-src
                         (+ ws)
                         lang
                         nl
                         block-content
                         end-src
                         (* ws)
                         (or nl eof)))

(define-peg-pattern block-content all
                    (* (and (not-followed-by end-src) peg-any)))

;; Paragraphs (Fallback: lines not starting with special markup)
(define-peg-pattern special-starts none
                    (or "*"
                        "-"
                        "+"
                        "#+"
                        (and (+ digit)
                             (or "." ")"))
                        (and (* ws) nl)))

(define-peg-pattern paragraph all
                    (and (not-followed-by special-starts) line-content
                         (or nl eof)))

(define-peg-pattern line-content all
                    (* (and (not-followed-by (or nl eof)) peg-any)))

;; =====================================================================
;; Inline Org Markup
;; =====================================================================

;; Escaped characters
;;
;; Org uses a backslash to escape markup characters:
;;
;;   \*not bold*
;;   \_not underline_
;;   \~not code~
;;
(define-peg-pattern inline-escape body
                    (and "\\" inline-escaped-character))

(define-peg-pattern inline-escaped-character all
                    (or "*"
                        "/"
                        "_"
                        "~"
                        "="
                        "["
                        "]"
                        "\\"))

;; Code: ~code~
(define-peg-pattern inline-code-delimiter none "~")

(define-peg-pattern inline-code-content all
                    (+ (and (not-followed-by "~")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-code body
                    (and inline-code-delimiter inline-code-content
                         inline-code-delimiter))

;; Verbatim: =verbatim=
(define-peg-pattern inline-verbatim-delimiter none "=")

(define-peg-pattern inline-verbatim-content all
                    (+ (and (not-followed-by "=")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-verbatim body
                    (and inline-verbatim-delimiter inline-verbatim-content
                         inline-verbatim-delimiter))

;; Italic: /italic/
(define-peg-pattern inline-italic-delimiter none "/")

(define-peg-pattern inline-italic-content all
                    (+ (and (not-followed-by "/")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-italic body
                    (and inline-italic-delimiter inline-italic-content
                         inline-italic-delimiter))

;; Bold: *bold*
(define-peg-pattern inline-bold-delimiter none "*")

(define-peg-pattern inline-bold-content all
                    (+ (and (not-followed-by "*")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-bold body
                    (and inline-bold-delimiter inline-bold-content
                         inline-bold-delimiter))

;; Underline: _underline_
(define-peg-pattern inline-underline-delimiter none "_")

(define-peg-pattern inline-underline-content all
                    (+ (and (not-followed-by "_")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-underline body
                    (and inline-underline-delimiter inline-underline-content
                         inline-underline-delimiter))

;; Org links:
;;
;;   [[https://example.com][Example]]
;;   [[https://example.com]]
;;
(define-peg-pattern link-open none "[[")
(define-peg-pattern link-close none "]]")
(define-peg-pattern link-sep none "][")

(define-peg-pattern link-target-content all
                    (+ (and (not-followed-by "]]")
                            (not-followed-by "][")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern link-description-content all
                    (+ (and (not-followed-by "]]")
                            (not-followed-by nl) peg-any)))

(define-peg-pattern inline-link-with-desc all
                    (and link-open link-target-content link-sep
                         link-description-content link-close))

(define-peg-pattern inline-link-no-desc all
                    (and link-open link-target-content link-close))

;; Plain inline text.
;;
;; Stop before the beginning of any inline construct.
(define-peg-pattern inline-text all
                    (+ (and (not-followed-by (or "\\"
                                                 "~"
                                                 "="
                                                 "/"
                                                 "*"
                                                 "_"
                                                 "[["
                                                 nl
                                                 eof)) peg-any)))

;; One inline element.
(define-peg-pattern inline-element body
                    (or inline-escape
                        inline-link-with-desc
                        inline-link-no-desc
                        inline-code
                        inline-verbatim
                        inline-italic
                        inline-bold
                        inline-underline
                        inline-text))

;; A complete inline sequence.
(define-peg-pattern inline-content body
                    (* inline-element))

;; =====================================================================
;; Parser Entry Point
;; =====================================================================

(define (inline-org->ast text)
  "Parse inline Org markup in TEXT."
  (let ((ast (peg:tree (match-pattern inline-content text))))
    (newline)
    (pretty-print ast)
    (newline) ast))

(define (org->ast str)
  "Convert an Org-mode string directly into an AST with nested list structures."
  (let ((ast (peg:tree (match-pattern org-doc str))))
    (newline)
    (pretty-print ast)
    (newline) ast))
