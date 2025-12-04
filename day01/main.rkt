#lang racket

(define (parse-input path)
  (file->lines path))

(define dir->sign (hash #\L -1 #\R 1))
(define (wrap n [modulus 100])
  (modulo n modulus))

(define (rotate-dial current delta)
  (wrap (+ current delta)))

(define (parse-line line)
  (define direction (string-ref line 0))
  (define count (string->number (substring line 1)))
  (define sign
    (hash-ref dir->sign direction (λ () (error 'parse-line '"Unknown direction ~a" direction))))
  (* sign count))

(define (bool->int predicate?)
  (if predicate? 1 0))

(define (part1 lines #:start [start 50])
  (define-values (_position zero-count)
    (for/fold ([position start]
               [zero-count 0])
              ([line lines])
      (define delta (parse-line line))
      (define new-position (rotate-dial position delta))

      (values new-position (+ zero-count (bool->int (zero? new-position))))))
  zero-count)

(define (crosses-zero? r position)
  (and (not (zero? position)) (let ([n (+ position r)]) (or (<= n 0) (>= n 100)))))

(define (part2 lines #:start [start 50] #:modulus [modulus 100])
  (define-values (_position zero-count)
    (for/fold ([position start]
               [zero-count 0])
              ([line lines])
      (define delta (parse-line line))
      (define q (quotient delta modulus))
      (define r (remainder delta modulus))

      (define new-position (rotate-dial position delta))
      (define new-zero-count
        (+ (abs q) (bool->int (or (zero? new-position) (crosses-zero? r position))) zero-count))

      (values new-position new-zero-count)))
  zero-count)

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? example1 3)
  (check-equal? example2 6))

(module+ main
  (define input (parse-input "day01/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
