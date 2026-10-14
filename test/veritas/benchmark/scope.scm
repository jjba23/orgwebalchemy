;; orgwebalchemy

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

(define-module (veritas benchmark scope)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (veritas veritas)
  #:use-module (orgwebalchemy peg)
  #:use-module (orgwebalchemy html)
  #:use-module (orgwebalchemy markdown))

;; =======================================================================
;; Running the benchmarks
;;
;; These are kept out of the unit run.  Run them with:
;;
;;   VERITAS_TEST_TYPE=benchmark \
;;   guix shell -m manifest.scm -- guile -L ./src -L ./test \
;;     -c '((@ (veritas runner) run-tests))'
;;
;; Veritas profiles each thunk with Guile's `statprof' (`#:runs' times), and
;; a deterministic wall-clock summary is printed afterwards, so the numbers
;; in the README can be reproduced and compared on another machine.
;; =======================================================================

(define (slurp path)
  (call-with-input-file path
    read-string))

(define feature-document
  (slurp "test/fixtures/sample-org-features.org"))

(define readme-document
  (slurp "README.org"))

(define prose-paragraph
  (string-join (make-list 50
                "This line has *bold* and /italic/ and ~code~ and a link [[https://gnu.org][GNU]].")
               " "))

(define (repeat n thunk)
  "Call @var{thunk} @var{n} times, discarding any result."
  (let loop
    ((i 0))
    (when (< i n)
      (thunk)
      (loop (+ i 1)))))

;; =======================================================================
;; Veritas benchmarks (profiling view)
;;
;; Each thunk repeats a unit of work enough times that `statprof' records
;; samples, and `#:runs' profiles it several times.
;; =======================================================================

(define parse-feature-benchmark
  (benchmark #:name "parse: feature fixture (100x)"
             #:runs 5
             #:thunk (lambda ()
                       (repeat 100
                               (lambda ()
                                 (org->ast feature-document))))))

(define parse-readme-benchmark
  (benchmark #:name "parse: README (50x)"
             #:runs 5
             #:thunk (lambda ()
                       (repeat 50
                               (lambda ()
                                 (org->ast readme-document))))))

(define render-html-benchmark
  (benchmark #:name "render: HTML of feature fixture (100x)"
             #:runs 5
             #:thunk (lambda ()
                       (repeat 100
                               (lambda ()
                                 (org->html feature-document))))))

(define render-markdown-benchmark
  (benchmark #:name "render: Markdown of feature fixture (100x)"
             #:runs 5
             #:thunk (lambda ()
                       (repeat 100
                               (lambda ()
                                 (org->markdown feature-document))))))

(define parse-inline-benchmark
  (benchmark #:name "parse: inline markup paragraph (100x)"
             #:runs 5
             #:thunk (lambda ()
                       (repeat 100
                               (lambda ()
                                 (inline-org->ast prose-paragraph))))))

;; =======================================================================
;; Wall-clock summary
;; =======================================================================

(define (wall-clock iterations thunk)
  "Return the seconds taken by @var{iterations} calls of @var{thunk}."
  (gc)
  (let* ((start (gettimeofday))
         (_ (repeat iterations thunk))
         (end (gettimeofday)))
    (+ (- (car end)
          (car start))
       (/ (- (cdr end)
             (cdr start)) 1000000.0))))

;; Each workload is (label iterations thunk); the iteration counts are chosen
;; so every measurement takes a fraction of a second and is comparable.
(define workloads
  (list (list "Parse the feature fixture" 2000
              (lambda ()
                (org->ast feature-document)))
        (list "Parse README.org" 200
              (lambda ()
                (org->ast readme-document)))
        (list "Render the feature fixture to HTML" 2000
              (lambda ()
                (org->html feature-document)))
        (list "Render the feature fixture to Markdown" 2000
              (lambda ()
                (org->markdown feature-document)))
        (list "Parse a 50-clause inline paragraph" 2000
              (lambda ()
                (inline-org->ast prose-paragraph)))))

(define (report)
  "Print the wall-clock table as Markdown, for pasting into the README."
  (display "
<!-- wall-clock benchmark, generated by veritas benchmark scope -->
")
  (display "| Workload | Iterations | Total (ms) | Per operation (µs) |
")
  (display "|----------|-----------:|-----------:|-------------------:|
")
  (for-each (lambda (workload)
              (let* ((label (car workload))
                     (iterations (cadr workload))
                     (thunk (caddr workload))
                     (seconds (wall-clock iterations thunk)))
                (format #t
                        "| ~a | ~a | ~,2f | ~,2f |~%"
                        label
                        iterations
                        (* 1000 seconds)
                        (* 1000000
                           (/ seconds iterations))))) workloads))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               parse-feature-benchmark
               parse-readme-benchmark
               render-html-benchmark
               render-markdown-benchmark
               parse-inline-benchmark)
  (report))
