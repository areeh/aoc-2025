#lang racket

(provide binary-search-by)

(define (binary-search-by needle seq cmp)
  (define vec
    (cond
      [(vector? seq) seq]
      [(list? seq) (list->vector seq)]
      [else (error 'binary-search-by "expected list or vector, got ~a" seq)]))

  (define (loop lo hi)
    (if (> lo hi)
        #f
        (let* ([mid (quotient (+ lo hi) 2)]
               [val (vector-ref vec mid)]
               [ord (cmp needle val)])

          (case ord
            ['less (loop lo (sub1 mid))]
            ['equal mid]
            ['greater (loop (add1 mid) hi)]
            [else (error 'loop "cmp must return less, equal, greater, got ~a" ord)]))))
  (loop 0 (sub1 (vector-length vec))))

(module+ test
  (require rackunit)

  ;; Helper comparison function for numbers
  (define (number-cmp a b)
    (cond
      [(< a b) 'less]
      [(= a b) 'equal]
      [(> a b) 'greater]))

  ;; Helper comparison function for strings
  (define (string-cmp a b)
    (cond
      [(string<? a b) 'less]
      [(string=? a b) 'equal]
      [(string>? a b) 'greater]))

  ;; Tests for binary-search-by with vectors
  (test-case "binary-search-by: find element in vector"
    (check-equal? (binary-search-by 5 #(1 3 5 7 9) number-cmp) 2))

  (test-case "binary-search-by: find first element in vector"
    (check-equal? (binary-search-by 1 #(1 3 5 7 9) number-cmp) 0))

  (test-case "binary-search-by: find last element in vector"
    (check-equal? (binary-search-by 9 #(1 3 5 7 9) number-cmp) 4))

  (test-case "binary-search-by: element not in vector"
    (check-false (binary-search-by 6 #(1 3 5 7 9) number-cmp)))

  (test-case "binary-search-by: element too small"
    (check-false (binary-search-by 0 #(1 3 5 7 9) number-cmp)))

  (test-case "binary-search-by: element too large"
    (check-false (binary-search-by 10 #(1 3 5 7 9) number-cmp)))

  (test-case "binary-search-by: single element vector - found"
    (check-equal? (binary-search-by 42 #(42) number-cmp) 0))

  (test-case "binary-search-by: single element vector - not found"
    (check-false (binary-search-by 43 #(42) number-cmp)))

  (test-case "binary-search-by: empty vector"
    (check-false (binary-search-by 5 #() number-cmp)))

  ;; Tests for binary-search-by with lists
  (test-case "binary-search-by: find element in list"
    (check-equal? (binary-search-by 5 '(1 3 5 7 9) number-cmp) 2))

  (test-case "binary-search-by: find first element in list"
    (check-equal? (binary-search-by 1 '(1 3 5 7 9) number-cmp) 0))

  (test-case "binary-search-by: find last element in list"
    (check-equal? (binary-search-by 9 '(1 3 5 7 9) number-cmp) 4))

  (test-case "binary-search-by: element not in list"
    (check-false (binary-search-by 6 '(1 3 5 7 9) number-cmp)))

  (test-case "binary-search-by: single element list - found"
    (check-equal? (binary-search-by 42 '(42) number-cmp) 0))

  (test-case "binary-search-by: single element list - not found"
    (check-false (binary-search-by 43 '(42) number-cmp)))

  (test-case "binary-search-by: empty list"
    (check-false (binary-search-by 5 '() number-cmp)))

  ;; Tests with strings
  (test-case "binary-search-by: find string in vector"
    (check-equal? (binary-search-by "banana" #("apple" "banana" "cherry" "date") string-cmp) 1))

  (test-case "binary-search-by: string not in vector"
    (check-false (binary-search-by "orange" #("apple" "banana" "cherry" "date") string-cmp)))

  ;; Tests with larger sequences
  (test-case "binary-search-by: large vector - found"
    (let ([vec (list->vector (range 0 1000 2))])
      (check-equal? (binary-search-by 500 vec number-cmp) 250)))

  (test-case "binary-search-by: large vector - not found"
    (let ([vec (list->vector (range 0 1000 2))])
      (check-false (binary-search-by 501 vec number-cmp))))

  ;; Tests for error handling
  (test-case "binary-search-by: invalid sequence type"
    (check-exn exn:fail?
               (lambda () (binary-search-by 5 "not a sequence" number-cmp))))

  (test-case "binary-search-by: invalid comparison return value"
    (check-exn exn:fail?
               (lambda () (binary-search-by 5 #(1 3 5 7 9)
                                            (lambda (a b) 'invalid))))))
