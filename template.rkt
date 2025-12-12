#lang racket

(require profile)

(define (parse-input path)
  (file->string path))

(define (part1 input)
  0)

(define (part2 input)
  0)

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? example1 'TODO)
  (check-equal? example2 'TODO))

(module+ main
  (define input (parse-input "dayxx/inputs/input.txt"))

  (define run
    (λ ()
      (printf "Part 1: ~a\n" (part1 input))
      (printf "Part 2: ~a\n" (part2 input))))

  (define prof? (getenv "PROFILE"))

  (if prof?
      (begin
        (printf "Profiling...\n")
        (profile-thunk run))
      (run)))
