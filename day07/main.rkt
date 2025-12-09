#lang racket

(require math/array
         "../grid_helpers.rkt"
         srfi/13)

(define (int->char n)
  (case n
    [(0) #\.]
    [(1) #\^]
    [else (error 'int->char "expexted 0 or 1, got ~a" n)]))

(define (char->int ch)
  (case ch
    [(#\.) 0]
    [(#\S) 0] ; start parsed out separately
    [(#\^) 1]
    [else (error 'char->int "expected . or ^ , got ~a" ch)]))

(define (string->int-array2d str)
  (string->int-array2d/mapping str char->int))

(define (start-coord path)
  (define first-line (first (file->lines path)))
  (define col (string-index first-line #\S))
  (if col
      (vector 0 col)
      (error 'start-coord "no S found in first line")))

(define (parse-input path)
  (list (string->int-array2d (file->string path)) (start-coord path)))

(define (coords->mask arr coords)
  (let* ([ds (array-shape arr)]
         [mask (array->mutable-array (make-array ds 0))]
         [idxs (vector->array (list->vector coords))])

    (array-indexes-set! mask idxs (array 1))
    mask))

(define (overlay-beams arr coords)
  (overlay-where arr (coords->mask arr coords) #\|))

(define (occupied? arr idx)
  (> (array-ref arr idx) 0))

(define (coord+ coord delta)
  (vector-map + coord delta))

(define (in-bounds? arr coord)
  (define shape (array-shape arr))
  (and (= (vector-length coord) (vector-length shape))
       (for/and ([i (in-vector coord)]
                 [n (in-vector shape)])
         (and (<= 0 i) (< i n)))))

(define left #(0 -1))
(define right #(0 1))
(define down #(1 0))

(define (set-at arr coord v)
  (if (in-bounds? arr coord)
      (array-set! arr coord v)
      '())
  arr)

(define (num-paths-at coord arr visited)
  (cond
    [(not (in-bounds? arr coord)) 1]
    [else (array-ref visited coord)]))

(define (step-beam coord splitters visited)
  (let* ([coord (coord+ coord down)]
         [num-paths (num-paths-at coord splitters visited)])
    (cond
      [(> num-paths 0) num-paths]
      [(occupied? splitters coord)
       (define v
         (+ (step-beam (coord+ coord left) splitters visited)
            (step-beam (coord+ coord right) splitters visited)))
       (set-at visited coord v)
       v]
      [else
       (define v (step-beam coord splitters visited))
       (set-at visited coord v)
       v])))

(define (count-nonzero-intersections arr1 arr2)
  (array-count (λ (x y) (and (not (zero? x)) (not (zero? y)))) arr1 arr2))

(define (part1 input)
  (match-define (list splitters start) input)
  (define visited (array->mutable-array (make-array (array-shape splitters) 0)))
  (step-beam start splitters visited)

  #;(displayln visited)
  #;(displayln (array-ref visited start))
  #;(define base (array-map int->char splitters))
  #;(define overlaid (overlay-where base visited #\|))
  #;(displayln (char-array2d->string overlaid))
  (count-nonzero-intersections splitters visited))

(define (part2 input)
  (match-define (list splitters start) input)
  (define visited (array->mutable-array (make-array (array-shape splitters) 0)))
  (step-beam start splitters visited)

  (array-ref visited (coord+ start down)))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (let* ([arr (array #[#[0 0 0] #[0 0 0] #[0 0 0]])]
         [coords (list #(0 1) #(2 2))]
         [mask (coords->mask arr coords)])

    (check-equal? mask (array #[#[0 1 0] #[0 0 0] #[0 0 1]])))

  (let* ([grid (string->int-array2d "
...
.^.
...")]
         [base (array-map int->char grid)]
         [coords (list #(0 1) #(1 0) #(1 2) #(2 0) #(2 2))]
         [overlaid (overlay-beams base coords)])
    (check-grid-equal? (char-array2d->string overlaid) "
.|.
|^|
|.|
"))

  (check-equal? example1 21)
  (check-equal? example2 40))

(module+ main
  (define input (parse-input "day07/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
