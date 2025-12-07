#lang racket

(require "../sequence_helpers.rkt"
         "../parse.rkt")

(struct id-range (lo hi) #:transparent)

(define (make-id-range lo hi)
  (unless (<= lo hi)
    (error 'make-id-range "lo must be <= hi, got ~a - ~a" lo hi))
  (id-range lo hi))

(define (id-range-empty? r)
  (= (id-range-lo r) (id-range-hi r)))

(define (id-range-size r)
  (- (id-range-hi r) (id-range-lo r)))

(define (contains? r x)
  (and (>= x (id-range-lo r)) (< x (id-range-hi r))))

(define (intersection a b)
  (let ([lo (max (id-range-lo a) (id-range-lo b))]
        [hi (min (id-range-hi a) (id-range-hi b))])
    (if (< lo hi)
        (id-range lo hi)
        (id-range 0 0))))

(define (adjacent? a b)
  (or (= (id-range-lo a) (id-range-hi b)) (= (id-range-lo b) (id-range-hi a))))

(define (overlaps? a b)
  (and (< (id-range-lo a) (id-range-hi b)) (< (id-range-lo b) (id-range-hi a))))

(define (union-if-touching a b)
  (if (or (adjacent? a b) (overlaps? a b))
      (make-id-range (min (id-range-lo a) (id-range-lo b)) (max (id-range-hi a) (id-range-hi b)))
      #f))

(struct id-range-set (ranges) #:transparent)

(define (normalize-ranges rs)
  (define sorted (sort (filter (λ (r) (not (id-range-empty? r))) rs) < #:key id-range-lo))
  (cond
    [(empty? sorted) '()]
    [else
     (define acc
       (for/fold ([acc (list (car sorted))]) ([next (cdr sorted)])
         (define combined (union-if-touching (car acc) next))
         (if combined
             (cons combined (cdr acc))
             (cons next acc))))
     (reverse acc)]))

(define (make-id-range-set . rs)
  (id-range-set (normalize-ranges rs)))

(define (id-range-set-contains? x range-set)
  (define (cmp needle range)
    (cond
      [(< needle (id-range-lo range)) 'less]
      [(>= needle (id-range-hi range)) 'greater]
      [else 'equal]))

  (number? (binary-search-by x (id-range-set-ranges range-set) cmp)))

(define (string->integers str)
  (map string->number (string->lines str)))

(define (string->id-ranges str)
  (map (λ (line)
         (match (string-split line "-")
           [(list lo hi) (make-id-range (string->number lo) (add1 (string->number hi)))]))
       (string->lines str)))

(define (string->id-range-set str)
  (apply make-id-range-set (string->id-ranges str)))

(define (parse-input path)
  (process-paragraphs (string-trim (file->string path)) string->id-range-set string->integers))

(define (part1 input)
  (match input
    [(list range-set ids) (count (λ (x) (id-range-set-contains? x range-set)) ids)]))

(define (part2 input)
  (match input
    [(list range-set _ids) (for/sum ([r (id-range-set-ranges range-set)]) (id-range-size r))]))

(module+ test
  (require rackunit)

  ;; make-id-range edge cases
  (check-exn exn:fail? (λ () (make-id-range 10 1)))
  (check-equal? (id-range-empty? (make-id-range 5 5)) #t)

  ;; contains? boundary tests
  (check-equal? (contains? (make-id-range 1 10) 1) #t)
  (check-equal? (contains? (make-id-range 1 10) 10) #f)

  ;; intersection edge cases
  (check-equal? (intersection (make-id-range 1 10) (make-id-range 8 11)) (make-id-range 8 10))
  (check-equal? (intersection (make-id-range 1 10) (make-id-range 11 20)) (id-range 0 0))

  ;; adjacent? symmetry and overlap distinction
  (check-equal? (adjacent? (make-id-range 1 10) (make-id-range 10 20)) #t)
  (check-equal? (adjacent? (make-id-range 10 20) (make-id-range 1 10)) #t)
  (check-equal? (adjacent? (make-id-range 1 10) (make-id-range 5 15)) #f)

  ;; overlaps? boundary cases (adjacent but not overlapping)
  (check-equal? (overlaps? (make-id-range 1 10) (make-id-range 0 2)) #t)
  (check-equal? (overlaps? (make-id-range 0 2) (make-id-range 1 10)) #t)
  (check-equal? (overlaps? (make-id-range 1 10) (make-id-range 0 1)) #f)
  (check-equal? (overlaps? (make-id-range 0 1) (make-id-range 1 10)) #f)

  ;; union-if-touching edge cases
  (check-equal? (union-if-touching (make-id-range 1 10) (make-id-range 10 20)) (make-id-range 1 20))
  (check-equal? (union-if-touching (make-id-range 1 10) (make-id-range 11 20)) #f)

  ;; make-id-range-set: complex multi-merge scenario
  (check-equal? (make-id-range-set (make-id-range 1 5)
                                   (make-id-range 4 8)
                                   (make-id-range 10 15)
                                   (make-id-range 20 25)
                                   (make-id-range 14 21))
                (id-range-set (list (make-id-range 1 8) (make-id-range 10 25))))

  ;; id-range-set-contains?: boundary and gap tests with multiple ranges
  (check-equal? (id-range-set-contains? 10 (make-id-range-set (make-id-range 1 10))) #f)
  (check-equal? (id-range-set-contains? 3
                                        (make-id-range-set (make-id-range 1 5) (make-id-range 10 15)))
                #t)
  (check-equal? (id-range-set-contains? 7
                                        (make-id-range-set (make-id-range 1 5) (make-id-range 10 15)))
                #f)
  (check-equal? (id-range-set-contains?
                 12
                 (make-id-range-set (make-id-range 1 5) (make-id-range 10 15) (make-id-range 20 25)))
                #t)

  ;; Example input tests
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (check-equal? example1 3)
  (check-equal? example2 14))

(module+ main
  (define input (parse-input "day05/inputs/input.txt"))

  (printf "Part 1: ~a\n" (part1 input))
  (printf "Part 2: ~a\n" (part2 input)))
