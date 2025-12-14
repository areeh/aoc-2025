#lang racket

(require math/array)

(define (lines->integer-array lines)
  (define rows
    (for/list ([line lines])
      (map string->number (string-split line ","))))
  (list*->array rows integer?))

(define (parse-input path)
  (lines->integer-array (file->lines path)))

(define (pairwise-euclidean-sq coords)
  ; pairwise euclidian from sklearn
  (match-let* ([(vector n d) (array-shape coords)]
               [norms (array-axis-sum (array-sqr coords) 1)]
               [G (array-axis-sum (array* (array-reshape coords (vector n 1 d))
                                          (array-reshape coords (vector 1 n d)))
                                  2)]
               [norms-col (array-reshape norms (vector n 1))]
               [norms-row (array-reshape norms (vector 1 n))])
    (array+ norms-col norms-row (array-scale G -2.0))))

(define (pairwise-euclidean coordinates)
  (array-sqrt (pairwise-euclidean-sq coordinates)))

(struct pair-dist (dist i j) #:transparent)

(define (unique-pairs distances)
  (match-define (vector n _) (array-shape distances))
  (for*/list ([i (in-range n)]
              [j (in-range (add1 i) n)])
    (pair-dist (array-ref distances (vector i j)) i j)))

(define (all-closest-pairs coordinates)
  (let* ([distances (pairwise-euclidean coordinates)]
         [pairs (unique-pairs distances)]
         [sorted (sort pairs < #:key pair-dist-dist)])
    (for/list ([p (in-list sorted)])
      (list (pair-dist-i p) (pair-dist-j p)))))

(define (merge-touching circuits edge)
  (match-define (list i j) edge)
  (define-values (touching rest)
    (partition (λ (s) (or (set-member? s i) (set-member? s j))) circuits))
  (define merged
    (if (null? touching)
        (set i j)
        (apply set-union (set i j) touching)))
  (cons merged rest))

(define (part1 input k)
  (let* ([pairs (all-closest-pairs input)]
         [circuits (for/fold ([circuits '()]) ([pair (take pairs k)])
                     (merge-touching circuits pair))]
         [top-circuits (take (sort circuits > #:key set-count) 3)])
    (apply * (map set-count top-circuits))))

(define (edge-when-fully-connected pairs n-coords)
  (define circuits '())
  (for/or ([pair pairs])
    (set! circuits (merge-touching circuits pair))
    (and (= (set-count (first circuits)) n-coords) pair)))

(define (part2 input)
  (match-let* ([(vector n _) (array-shape input)]
               [pairs (all-closest-pairs input)]
               [last-edge (edge-when-fully-connected pairs n)])
    (unless last-edge
      (error 'part2 "checked all pairs without fully connecting the graph"))
    (apply * (map (λ (row) (array-ref input (vector row 0))) last-edge))))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt") 10))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? example1 40)
  (check-equal? example2 25272))

(module+ main
  (define input (parse-input "day08/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input 1000))
  (printf "Part 2: ~a\n" (part2 input)))
