#lang racket

(define (file->digit-lines path)
  (map (λ (line)
         (unless (regexp-match? #px"^[0-9]+$" line)
           (error 'file->digit-lines "line contains non-digit characters: ~a" line))
         (map (λ (ch) (- (char->integer ch) (char->integer #\0))) (string->list line)))
       (file->lines path)))

(define (parse-input path)
  (file->digit-lines path))

(define (top-sequence-2 xs)
  (when (< (length xs) 2)
    (error 'top-sequence-2 "sequence must have length at least 2"))

  (let* ([last-elem (last xs)]
         [prefix (drop-right xs 1)])
    (define-values (left right)
      (for/fold ([left 0]
                 [right 0])
                ([i prefix])
        (cond
          [(> i left) (values i 0)]
          [(> i right) (values left i)]
          [else (values left right)])))
    (cons left (if (> last-elem right) last-elem right))))

(define (2-digits->integer pair)
  (match pair
    [(cons tens ones) (+ (* tens 10) ones)]))

(define (best-jolts xs)
  (2-digits->integer (top-sequence-2 xs)))

(define (part1 digit-sequences)
  (for/sum ([xs digit-sequences]) (best-jolts xs)))

(define (index-max xs)
  (for/fold ([i-mx -1]
             [mx 0]
             #:result i-mx)
            ([v xs]
             [i (in-naturals)])
    (if (> v mx)
        (values i v)
        (values i-mx mx))))

(define (lexicographic-max-subsequence xs k)
  (let loop ([xs xs]
             [k k])
    (cond
      [(zero? k) '()]
      [else
       (let* ([n (length xs)]
              [prefix-len (add1 (- n k))]
              [prefix (take xs prefix-len)]
              [i-mx (index-max prefix)]
              [mx (list-ref xs i-mx)]
              [xs* (drop xs (add1 i-mx))])
         (cons mx (loop xs* (sub1 k))))])))

(define (digits-seq->integer seq)
  (for/fold ([n 0]) ([v seq])
    (+ (* n 10) v)))

(define (part2 digit-sequences)
  (for/sum ([xs digit-sequences]) (digits-seq->integer (lexicographic-max-subsequence xs 12))))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? (best-jolts '(1 2 3 4 3 4 3 1)) 44)
  (check-equal? (best-jolts '(1 2 3 4 3 4 3 9)) 49)

  (check-equal? example1 357)

  (check-equal? (index-max '(1 2 3 4 3)) 3)
  (check-equal? (index-max '(1 2 3 4 3 4)) 3)

  (check-equal? (lexicographic-max-subsequence '(1 2 3 4 3) 2) '(4 3))
  (check-equal? (digits-seq->integer '(1 3 2)) 132)

  (check-equal? example2 3121910778619))

(module+ main
  (define input (parse-input "day03/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
