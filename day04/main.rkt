#lang racket

(require math/array
         "../grid_helpers.rkt")

(define (int->char n)
  (case n
    [(0) #\.]
    [(1) #\@]
    [else (error 'int->char "expected 0 or 1, got ~a" n)]))

(define (char->int ch)
  (match ch
    [#\. 0]
    [#\@ 1]
    [_ (error 'char->int "expected . or @ , got ~a" ch)]))

(define (accessible-rolls arr)
  (let* ([padded (pad-n arr 1 0)]
         [counts (padded2d-neighbor padded
                                    neighbor-offsets/8
                                    +
                                    #:cell-pred (λ (cell) (= cell 1))
                                    #:default-val 9)])
    (array-map (λ (n) (if (< n 4) 1 0)) counts)))

(define (char->int-array2d str)
  (string->int-array2d/mapping str char->int))

(define (parse-input path)
  (char->int-array2d (file->string path)))

(define (part1 input)
  (array-all-sum (accessible-rolls input)))

(define (remove-accessible arr accessible)
  (array* arr (array- (array 1) accessible)))

(define (count-removable arr)
  (let loop ([arr arr]
             [count 0])
    (let* ([accessible (accessible-rolls arr)]
           [n-accessible (array-all-sum accessible)])
      (if (> n-accessible 0)
          (loop (remove-accessible arr accessible) (+ count n-accessible))
          count))))

(define (part2 input)
  (count-removable input))

(module+ test
  (require rackunit)

  (let ([grid (char->int-array2d "
@.@
.@.
@.@")])
    (check-grid-equal? (char-array2d->string grid #:elem->char int->char) "
@.@
.@.
@.@
"))

  (let* ([grid (char->int-array2d "
@.@
.@.
@.@")]
         [base (array-map int->char grid)]
         [mask (char->int-array2d "
.@.
@.@
.@.")]
         [overlaid (overlay-where base mask #\x)])
    (check-grid-equal? (char-array2d->string overlaid) "
@x@
x@x
@x@
"))

  (let ([grid (char->int-array2d "
@.@
..@
@@.")])
    (check-grid-equal? (char-array2d->string grid #:elem->char integer->digit) "
101
001
110
"))

  (let ([result
         (char->int-array2d
          "
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
")])
    (check-grid-equal?
     (char-array2d->string (overlay-where (array-map int->char result) (accessible-rolls result) #\x))
     "
..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x.
"))

  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? example1 13)
  (check-equal? example2 43))

(module+ main
  (define input (parse-input "day04/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
