;;; demo.scm --- render examples/hello.org with OrgWebAlchemy

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; Run it from the repository root:
;;
;;   guix shell -m manifest.scm -- guile -L ./src examples/demo.scm

(use-modules (ice-9 textual-ports)
             (orgwebalchemy html)
             (orgwebalchemy markdown))

(define here
  (dirname (current-filename)))

(define org-source
  (call-with-input-file (string-append here "/hello.org")
    get-string-all))

(display ";; HTML fragment\n")
(display (org->html org-source))
(newline)
(newline)
(display ";; Markdown\n")
(display (org->markdown org-source))
