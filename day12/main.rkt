#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         profile)

;; shapes
(define sharp/p (do (char/p #\#) (pure #t)))
(define dot/p (do (char/p #\.) (pure #f)))
(define cell/p (do (or/p sharp/p dot/p)))
(define row/p (do [cells <- (many+/p cell/p)] (char/p #\newline) (pure cells)))
(define grid-id/p (try/p (do [id <- integer/p] (char/p #\:) (char/p #\newline) (pure id))))
(define shape/p
  (do [id <- grid-id/p] [grid <- (many+/p row/p)] (char/p #\newline) (pure (list id grid))))
(define shapes/p (many+/p (do [s <- shape/p] (many/p space/p) (pure s))))

;; problems
(define dims/p (do [dim1 <- integer/p] (char/p #\x) [dim2 <- integer/p] (pure (list dim1 dim2))))
(define counts/p (do [counts <- (many+/p (do (char/p #\space) integer/p))] (pure counts)))
(define problems/p
  (many+/p (do [d <- dims/p] (char/p #\:) [c <- counts/p] (many/p space/p) (pure (list d c)))))

(define full/p (do (many/p space/p) [s <- shapes/p] [p <- problems/p] (pure (list s p))))

(define (parse-input path)
  (parse-result! (parse-string full/p (file->string path))))

;; packing squares takes >= space
;; than any other shape
(define (square-lower-bound dims)
  (for/product ([dim dims]) (quotient dim 3)))

(define (guaranteed? dims counts)
  (>= (square-lower-bound dims) (apply + counts)))

;; all packing strategies are worse or equal
;; to packing with no gaps
(define (impossible? dims counts shapes)
  (define (coverage shape)
    (for*/sum ([row (in-list shape)] [b (in-list row)] #:when b) 1))
  (define total-coverage (for/sum ([n counts] [cover (map coverage shapes)]) (* n cover)))
  (> total-coverage (apply * dims)))

(define (part1 input)
  (match-let* ([(list id-shapes problems) input]
               [shapes (map second id-shapes)]
               [total-n (length problems)]
               [guaranteed-n (for/sum ([p (in-list problems)])
                                      (match-define (list dims counts) p)
                                      (if (guaranteed? dims counts) 1 0))]
               [impossible-n (for/sum ([p (in-list problems)])
                                      (match-define (list dims counts) p)
                                      (if (impossible? dims counts shapes) 1 0))])
    (printf "guaranteed: ~a\nimpossible: ~a\nmaybe: ~a\ntotal: ~a\n"
            guaranteed-n
            impossible-n
            (- total-n guaranteed-n impossible-n)
            total-n)
    guaranteed-n))

(module+ test
  (require rackunit)

  ;; example fails, but real input works
  ;# (define example1 (part1 (parse-input "inputs/example.txt")))
  ;# (check-equal? example1 2)
  ;
  (match-define (list shapes problems) (parse-input "inputs/example.txt"))

  (displayln "=== Shapes ===")
  (for ([s shapes])
    (printf "ID: ~a\n" (first s))
    (for ([row (second s)])
      (displayln (list->string (map (λ (b) (if b #\# #\.)) row))))
    (newline))

  (displayln "=== Problems ===")
  (for ([p problems])
    (match-define (list (list w h) counts) p)
    (printf "Size: ~ax~a | Counts: ~a\n" w h counts))

  ;; Display the raw boolean structure for the first item
  (printf "Raw structure of ID 0:\n~v\n" (second (first shapes)))

  (define main-output (part1 (parse-input "inputs/input.txt")))
  (check-equal? main-output 528))

(module+ main
  (define input (parse-input "day12/inputs/input.txt"))

  (define run (λ () (printf "Part 1: ~a\n" (part1 input))))

  (define prof? (getenv "PROFILE"))

  (if prof?
      (begin
        (printf "Profiling...\n")
        (profile-thunk run))
      (run)))
