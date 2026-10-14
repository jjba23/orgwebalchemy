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
;; Measurements
;; =======================================================================

(define (seconds-between start end)
  "Return the seconds between two @code{gettimeofday} values."
  (+ (- (car end)
        (car start))
     (/ (- (cdr end)
           (cdr start)) 1000000.0)))

(define (wall-clock iterations thunk)
  "Return the seconds taken by @var{iterations} calls of @var{thunk}."
  (gc)
  (let ((start (gettimeofday)))
    (repeat iterations thunk)
    (seconds-between start
                     (gettimeofday))))

(define (gc-stat stats key)
  "Return @var{key} from a @code{gc-stats} alist, or 0 when it is absent."
  (or (assq-ref stats key) 0))

(define (measure-allocation iterations thunk)
  "Measure what @var{iterations} calls of @var{thunk} do to the heap.
The heap is collected first, so the numbers describe this workload and nothing
else.  Returns @code{(bytes-per-call gc-runs-per-thousand gc-ms-per-call)}.

Allocation is `heap-total-allocated' -- everything the workload asked for,
including the garbage it immediately dropped -- so it measures allocation
pressure, not the memory the result keeps.

The GC time is Guile's @code{gc-time-taken}, and Guile counts collector *CPU*
time: the marker threads' time is summed, so one full collection of a large
heap is reported as several times its wall-clock cost on a multi-core machine.
Read it as `how much collector work this costs', not as a share of the clock."
  (gc)
  (let* ((before (gc-stats))
         (start (gettimeofday))
         (_ (repeat iterations thunk))
         (after (gc-stats))
         (bytes (- (gc-stat after
                            'heap-total-allocated)
                   (gc-stat before
                            'heap-total-allocated)))
         (runs (- (gc-stat after
                           'gc-times)
                  (gc-stat before
                           'gc-times)))
         (collected (- (gc-stat after
                                'gc-time-taken)
                       (gc-stat before
                                'gc-time-taken))))
    (list (/ bytes iterations)
          (/ (* 1000.0 runs) iterations)
          (/ (/ collected 1000000.0) iterations))))

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

(define attempts
  ;; A single run is noisy enough (other processes, where the heap happens to
  ;; be in its cycle) to move a figure by 10%.  Each workload is run several
  ;; times and the fastest run is reported: the other runs were interrupted by
  ;; something, this one was not.
  5)

(define (best-run iterations thunk)
  "Return @code{(seconds allocation)} for the fastest of @var{attempts} runs."
  (let loop
    ((left attempts)
     (best #f))
    (if (= left 0) best
        (let ((seconds (wall-clock iterations thunk))
              (allocation (measure-allocation iterations thunk)))
          (loop (- left 1)
                (if (or (not best)
                        (< seconds
                           (car best)))
                    (list seconds allocation) best))))))

(define (report)
  "Print the wall-clock and allocation tables as Markdown, for the README."
  (define results
    (map (lambda (workload)
           (let* ((label (car workload))
                  (iterations (cadr workload))
                  (thunk (caddr workload))
                  (best (best-run iterations thunk)))
             (list label iterations
                   (car best)
                   (cadr best)))) workloads))
  (display "
<!-- wall-clock benchmark, generated by veritas benchmark scope -->
")
  (display "| Workload | Iterations | Total (ms) | Per operation (µs) |
")
  (display "|----------|-----------:|-----------:|-------------------:|
")
  (for-each (lambda (result)
              (let ((label (car result))
                    (iterations (cadr result))
                    (seconds (caddr result)))
                (format #t
                        "| ~a | ~a | ~,2f | ~,2f |~%"
                        label
                        iterations
                        (* 1000 seconds)
                        (* 1000000
                           (/ seconds iterations))))) results)
  (display "
<!-- allocation benchmark, generated by veritas benchmark scope -->
")
  (display
   "| Workload | Allocated per operation | GC runs per 1000 ops | GC time per operation |
")
  (display
   "|----------|------------------------:|---------------------:|----------------------:|
")
  (for-each (lambda (result)
              (let ((label (car result))
                    (allocation (cadddr result)))
                (format #t
                        "| ~a | ~,2f KiB | ~,2f | ~,2f ms |~%"
                        label
                        (/ (car allocation) 1024.0)
                        (cadr allocation)
                        (caddr allocation)))) results))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               parse-feature-benchmark
               parse-readme-benchmark
               render-html-benchmark
               render-markdown-benchmark
               parse-inline-benchmark)
  (report))
