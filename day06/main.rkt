#lang racket

(require math/array
         "../parse.rkt"
         "../grid_helpers.rkt")

(define (lines->array lines)
  (define rows
    (for/list ([line lines])
      (map string->number (string-split line))))
  (list*->array rows integer?))

(define (string->problems str)
  (lines->array (drop-right (string->lines (string-trim str)) 1)))

(define (indices-where pred? seq)
  (for/list ([(v i) (in-indexed seq)]
             #:when (pred? v))
    i))

(define (parse-input path)
  (define arr (string->problems (string-trim (file->string path))))
  (define op-line (last (string->lines (string-trim (file->string path)))))
  (define ops (string-split op-line))
  (cons arr ops))

(define (part1 input)
  (match input
    [(cons arr ops)
     (define prod-arr
       (array-slice-ref arr (list (::) (indices-where (lambda (ch) (string=? ch "*")) ops))))
     (define sum-arr
       (array-slice-ref arr (list (::) (indices-where (lambda (ch) (string=? ch "+")) ops))))
     (+ (array-all-sum (array-axis-prod prod-arr 0)) (array-all-sum sum-arr))]))

(define (char-array->integer arr)
  (define str (list->string (array->list arr)))
  (string->number (string-trim str)))

(define (map-while f pred? seq)
  (for/fold ([acc '()]
             #:result (reverse acc))
            ([x seq])
    (define y (f x))
    #:break (not (pred? y))
    (cons y acc)))

(define (all-whitespace?/array arr)
  (array-andmap char-whitespace? arr))

(define (col-take-until arr col-indexes rows-spec stop?)
  (map-while (λ (j) (array-slice-ref arr (list rows-spec j)))
             (λ (col) (not (stop? col)))
             col-indexes))

(define (numbers-in-block start-col arr cols)
  (define columns (col-take-until arr (in-range start-col cols) (::) all-whitespace?/array))
  (map char-array->integer (reverse columns)))

(define (char->op ch)
  (case ch
    [(#\*) *]
    [(#\+) +]
    [else (error 'char->op '"unknown op ~a, expected one of * +" ch)]))

(define (op-indexed ops)
  (for/list ([(ch i) (in-indexed ops)]
             #:when (not (char-whitespace? ch)))
    (cons (char->op ch) i)))

(define (part2 input)
  (let* ([arr (string->char-array2d input)]
         [shape (array-shape arr)]
         [rows (vector-ref shape 0)]
         [cols (vector-ref shape 1)]
         [num-rows-spec (:: 0 (sub1 rows))])

    (define number-arr (array-slice-ref arr (list num-rows-spec (::))))
    (define op-arr (array-slice-ref arr (list (sub1 rows) (::))))
    (define op-locs (op-indexed (array->list op-arr)))

    (define answers
      (map (match-lambda
             [(cons op j) (apply op (numbers-in-block j number-arr cols))])
           op-locs))

    (apply + answers)))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (file->string "inputs/example.txt")))

  (let* ([input "
     1  2 3
    7   8   9
    3 1 2
    * + /
  "]
         [arr (string->problems input)])
    (check-grid-equal? (char-array2d->string arr #:elem->char integer->digit)
                       "
     123
     789
     312"))

  ;; basic case: list input, stops when (pred? (f x)) becomes false
  (check-equal? (map-while (λ (x) (* x x)) (λ (y) (< y 20)) '(1 2 3 4 5 6)) '(1 4 9 16))

  ;; all elements satisfy predicate => same as full map
  (check-equal? (map-while add1 (λ (y) (< y 10)) '(1 2 3 4)) '(2 3 4 5))

  ;; first element already fails => empty result
  (check-equal? (map-while add1 (λ (y) (< y 0)) '(1 2 3)) '())

  ;; works on arbitrary sequences (e.g. in-range)
  (check-equal? (map-while (λ (x) (* x x)) (λ (y) (<= y 25)) (in-range 0 10)) '(0 1 4 9 16 25))

  ;; vector as a sequence
  (check-equal? (map-while (λ (x) (* 2 x)) (λ (y) (<= y 6)) #(1 2 3 4 5)) '(2 4 6))

  (let* ([input "
   123   1
    45  23
     6 456
   *   +  "]
         [arr (string->char-array2d input)]
         [shape (array-shape arr)]
         [rows (vector-ref shape 0)]
         [cols (vector-ref shape 1)]
         ;; ignore last row containing the operators
         [rows-spec (:: 0 (sub1 rows))])

    ;; extract block starting at column j
    (define number-arr (array-slice-ref arr (list rows-spec (::))))
    (define op-arr (array-slice-ref arr (list (sub1 rows) (::))))
    (define op-locs (op-indexed (array->list op-arr)))

    (define (grab j)
      (numbers-in-block j number-arr cols))

    (define b1 (grab 0))
    (define b2 (grab 4))

    (check-equal? b1 (list 356 24 1))
    (check-equal? b2 (list 136 25 4))

    (define (solve-problems)
      (map (match-lambda
             [(cons op j) (apply op (grab j))])
           op-locs))

    (check-equal? (solve-problems) (list 8544 165)))

  (check-equal? example1 4277556)
  (check-equal? example2 3263827))

(module+ main
  (define path "day06/inputs/input.txt")
  (define input (parse-input path))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 (file->string path))))
