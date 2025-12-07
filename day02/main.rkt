#lang racket

(define (parse-input path)
  (string-trim (file->string path)))

(define (invalid? id)
  (define n (string-length id))
  (define mid (quotient n 2))
  (and (even? n) (string=? (substring id 0 mid) (substring id mid n))))

(define (sum-range-matching lo hi pred?)
  (for/sum ([i (in-range lo (add1 hi))] #:when (pred? (number->string i))) i))

(define (parse-range range-string)
  (match (string-split range-string "-")
    [(list lo hi) (values (string->number lo) (string->number hi))]
    [_ (error 'parse-range "expected format 'lo-hi', got ~a" range-string)]))

(define (sum-line line pred?)
  (for/sum ([range-string (string-split line ",")])
           (define-values (lo hi) (parse-range range-string))
           (sum-range-matching lo hi pred?)))

(define (part1 input)
  (sum-line input invalid?))

(define (repeats-prefix? str prefix-len)
  (define n (string-length str))
  (and (>= (quotient n prefix-len) 2)
       (zero? (remainder n prefix-len))
       (for/and ([i [in-range prefix-len n]])
         (char=? (string-ref str i) (string-ref str (remainder i prefix-len))))))

(define (any-prefix-repeats? str)
  (define n (string-length str))
  (for/or ([prefix-len (in-range 1 n)])
    (repeats-prefix? str prefix-len)))

(define (part2 input)
  (sum-line input any-prefix-repeats?))

(module+ test
  (require rackunit)

  (check-equal? (invalid? "662661") #f)
  (check-equal? (invalid? "66266") #f)
  (check-equal? (invalid? "662662") #t)

  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? (repeats-prefix? "663663662" 3) #f)
  (check-equal? (repeats-prefix? "663663663" 3) #t)

  (check-equal? example1 1227775554)
  (check-equal? example2 4174379265))

(module+ main
  (define input (parse-input "day02/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
