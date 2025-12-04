#lang racket

(require net/http-client
         html-parsing
         racket/date
         (only-in sxml sxpath))

;; Setup script for Advent of Code days
;; Usage: racket setup_puzzles.rkt

(define (get-max-day year)
  "Get maximum day for a given year (25 for <=2024, 12 for >=2025)"
  (if (>= year 2025) 12 25))

(define (get-current-day-and-year)
  "Get current day and year for AoC based on system date"
  (define now (current-date))
  (define year (date-year now))
  (define month (date-month now))
  (define day (date-day now))

  ;; Only valid during December
  (unless (= month 12)
    (error "Not December - AoC runs in December only"))

  ;; Cap day to year-appropriate maximum
  (define max-day (get-max-day year))
  (define capped-day (min day max-day))

  (values year capped-day))

(define (get-env-var name)
  "Get environment variable or error"
  (or (getenv name)
      (error (format "Environment variable ~a not set" name))))

(define (fetch-aoc-input year day session-cookie)
  "Fetch puzzle input from AoC"
  (define path (format "/~a/day/~a/input" year day))
  (define-values (status-line headers in)
    (http-sendrecv "adventofcode.com"
                   path
                   #:ssl? #t
                   #:headers (list (format "Cookie: session=~a" session-cookie))))
  (port->string in))

(define (fetch-puzzle-description year day session-cookie)
  "Fetch puzzle description HTML"
  (define path (format "/~a/day/~a" year day))
  (define-values (status-line headers in)
    (http-sendrecv "adventofcode.com"
                   path
                   #:ssl? #t
                   #:headers (list (format "Cookie: session=~a" session-cookie))))
  (port->string in))

(define (extract-code-snippets html-str)
  "Extract text content from <pre><code> blocks only"
  (define xexpr (html->xexp html-str))
  (define code-blocks ((sxpath "//pre/code") xexpr))

  (for/list ([node code-blocks])
    (string-join (filter string? (flatten node)) "")))

(define (setup-day year day session-cookie)
  "Set up directory structure and files for a given day"
  (define day-str (format "day~a" (~r day #:min-width 2 #:pad-string "0")))
  (define day-dir (build-path (current-directory) day-str))
  (define inputs-dir (build-path day-dir "inputs"))

  ;; Create directories
  (unless (directory-exists? day-dir)
    (make-directory day-dir)
    (printf "Created directory: ~a\n" day-str))

  (unless (directory-exists? inputs-dir)
    (make-directory inputs-dir)
    (printf "Created directory: ~a/inputs\n" day-str))

  ;; Copy template to main.rkt
  (define template-file (build-path (current-directory) "template.rkt"))
  (define main-file (build-path day-dir "main.rkt"))
  (unless (file-exists? main-file)
    (copy-file template-file main-file)
    (printf "Created ~a/main.rkt\n" day-str))

  ;; Fetch and save input.txt
  (define input-file (build-path inputs-dir "input.txt"))
  (unless (file-exists? input-file)
    (printf "Fetching input.txt...\n")
    (define input-content (fetch-aoc-input year day session-cookie))
    (call-with-output-file input-file
      (lambda (out) (display input-content out)))
    (printf "Created ~a/inputs/input.txt\n" day-str))

  ;; Check if any example files exist
  (define existing-examples
    (filter (lambda (f)
              (and (string-prefix? (path->string (file-name-from-path f)) "example")
                   (string-suffix? (path->string (file-name-from-path f)) ".txt")))
            (if (directory-exists? inputs-dir)
                (directory-list inputs-dir #:build? #t)
                '())))

  ;; Only fetch puzzle description if no example files exist
  (when (empty? existing-examples)
    (printf "Fetching puzzle description...\n")
    (define html (fetch-puzzle-description year day session-cookie))
    (define snippets (extract-code-snippets html))

    (when (empty? snippets)
      (printf "No code snippets found in puzzle description\n"))

    ;; Create example files for each snippet
    (for ([snippet snippets]
          [i (in-naturals)])
      (define filename (if (= i 0)
                           "example.txt"
                           (format "example~a.txt" i)))
      (define example-file (build-path inputs-dir filename))
      (call-with-output-file example-file
        (lambda (out) (display snippet out)))
      (printf "Created ~a/inputs/~a\n" day-str filename)))

  (printf "\nDay ~a setup complete!\n" day))

(module+ main
  (define session-cookie (get-env-var "AOC_SESSION"))
  (define-values (year day) (get-current-day-and-year))

  (printf "Setting up Advent of Code ~a Day ~a\n" year day)
  (setup-day year day session-cookie))
