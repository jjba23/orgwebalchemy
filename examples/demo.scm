;;; demo.scm --- render examples/hello.org with OrgWebAlchemy

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; This file is part of orgwebalchemy and is licensed under the GNU Lesser
;; General Public License, version 3 or (at your option) any later version.

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
