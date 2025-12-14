#lang racket

(require profile
         megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(struct problem (lights toggles joltage width) #:transparent)

(define (bool->char b)
  (if b #\# #\.))

(define (falses n)
  (make-list n #f))

;; Pack bool list (length n) into an integer in [0, 2^n − 1]
(define (bool-list->int bs)
  (for/fold ([acc 0]) ([b (in-list bs)])
    (bitwise-ior (arithmetic-shift acc 1) (if b 1 0))))

;; Integer -> bool list of length n (big-endian)
(define (int->bool-list n x)
  (for/list ([i (in-range n)])
    (let* ([shift (- n 1 i)]
           [bit (bitwise-and 1 (arithmetic-shift x (- shift)))])
      (not (zero? bit)))))

(define (bits->string width bits)
  (define bools (int->bool-list width bits))
  (define chars
    (for/list ([v (in-list bools)])
      (bool->char v)))
  (string-append "[" (list->string chars) "]"))

(define bool-char/p (or/p (do (char/p #\#) (pure #t)) (do (char/p #\.) (pure #f))))

(define lights/p
  (do (char/p #\[)
      [bools <- (many+/p bool-char/p)]
      (char/p #\])
      (pure (cons (bool-list->int bools) (length bools)))))

(define int-list/p
  (do [first <- integer/p] [rest <- (many/p (do (char/p #\,) integer/p))] (pure (cons first rest))))

(define joltage/p (do (char/p #\{) [vals <- int-list/p] (char/p #\}) (pure vals)))

(define toggle/p (do (char/p #\() [vals <- int-list/p] (char/p #\)) (pure (sort vals <))))

(define line/p
  (do [lights-result <- lights/p]
      (many+/p space/p)
      [toggles <- (many+/p (do [t <- toggle/p] (many+/p space/p) (pure t)))]
      [joltage <- joltage/p]
      (or/p (char/p #\newline) eof/p)
      (match-let ([(cons lights-bits width) lights-result])
        (pure (problem lights-bits toggles joltage width)))))

(define input/p (many+/p line/p))

(define (parse-input path)
  (parse-result! (parse-string input/p (file->string path))))

(define (mark-true-at-indices bools idxs)
  (for/fold ([bs bools]) ([i idxs])
    (list-set bs i #t)))

(define (true-indices->bool idxs n)
  (mark-true-at-indices (falses n) idxs))

(define (get-children node toggles)
  (match-define (list state path) node)
  (for/list ([toggle toggles])
    (define new-state (bitwise-xor state toggle))
    (define new-path (cons toggle path))
    (list new-state new-path)))

(define (bfs start toggles)
  (define visited (make-hash))
  (let loop ([queue (list (list start '()))])
    (cond
      [(empty? queue) visited]
      [else
       (match-define (list state path) (first queue))
       (define rest-queue (rest queue))

       (if (hash-has-key? visited state)
           (loop rest-queue)
           (begin
             (hash-set! visited state (length path))
             (if (zero? state)
                 visited
                 (loop (append rest-queue (get-children (list state path) toggles))))))])))

(define (part1 input)
  (define distances
    (for/list ([prob input])
      (match-define (problem lights toggle-indexes _joltage width) prob)

      (define toggles
        (for/list ([idxs (in-list toggle-indexes)])
          (bool-list->int (true-indices->bool idxs width))))

      (define visited (bfs lights toggles))
      (hash-ref visited 0 #f)))
  (apply + distances))

(define (ceil-div a b)
  (quotient (+ a (sub1 b)) b))

;; Required presses if always using the worst button
(define (upper-bound target button-idxs)
  (ceil-div (apply + target) (apply min (map length button-idxs))))

(define (solve-problem prob)
  (match-let* ([(problem _ button-idxs target _) prob]
               [n (length target)]
               [ub (upper-bound target button-idxs)]
               [buttons (for/list ([b (in-list button-idxs)])
                          (for/list ([i (in-range n)])
                            (if (member i b) 1 0)))]
               ;; Precompute: Pattern Vector -> Min Cost (Iterate k 0..N)
               [patterns (for*/fold ([memo (hash)])
                                    ([k (in-range (add1 (length buttons)))]
                                     [combo (in-combinations buttons k)])
                           (define pat (apply map + (cons (make-list n 0) combo)))
                           (if (hash-has-key? memo pat)
                               memo
                               (hash-set memo pat k)))]
               [memo (make-hash)])
    (let solve ([goal target])
      (hash-ref!
       memo
       goal
       (thunk (if (andmap zero? goal)
                  0
                  (for/fold ([best ub]) ([(pat cost) (in-hash patterns)])
                    ;; Check bounds and parity
                    (if (andmap (λ (p g) (and (<= p g) (= (modulo p 2) (modulo g 2)))) pat goal)
                        (min best (+ cost (* 2 (solve (map (λ (p g) (/ (- g p) 2)) pat goal)))))
                        best))))))))

(define (part2 input)
  (for/sum ([prob input]) (solve-problem prob)))

(module+ test
  (require rackunit)

  (test-case "bool-list->int basic cases"
    (check-equal? (bool-list->int '()) 0)
    (check-equal? (bool-list->int '(#f)) 0)
    (check-equal? (bool-list->int '(#t)) 1)
    (check-equal? (bool-list->int '(#t #f #t #t)) #b1011))

  ;; Round-trip bool-list -> int -> bool-list
  (test-case "round-trip bool-list <-> int"
    (define bs '(#t #f #t #f #t #f #f))
    (define n (length bs))
    (define x (bool-list->int bs))
    (check-equal? (int->bool-list n x) bs))

  ;; Pretty printing format
  (test-case "bits->string format"
    (define bs '(#t #f #t #f #t #f #f))
    (define n (length bs))
    (define x (bool-list->int bs))
    (define pretty (bits->string n x))
    (check-equal? pretty "[#.#.#..]"))

  (test-case "pretty string <-> bits roundtrip"
    (define bs '(#t #f #t #f #t #f #f))
    (define n (length bs))
    (define x (bool-list->int bs))

    (define s (bits->string n x))
    (check-equal? s "[#.#.#..]")

    (match-define (cons x2 width) (parse-result! (parse-string lights/p s)))
    (check-equal? x2 x)
    (check-equal? width n)
    (check-equal? (int->bool-list n x2) bs))

  (test-case "xor pipeline"
    (define bs '(#t #f #t #f #t #f #f))
    (define n (length bs))
    (define x0 (bool-list->int bs))

    (define x1 #b0101010)
    (define x2 #b1110000)
    (define x3 #b0011001)

    (define r1 (bitwise-xor x0 x1))
    (define r2 (bitwise-xor r1 x2))
    (define r3 (bitwise-xor r2 x3))

    (define target-int #b0010111)
    (define same? (= r3 target-int))
    (check-true same?)
    (check-equal? (bits->string n r3) "[..#.###]"))

  (test-case "part1 example"
    (define example-input (parse-input "inputs/example.txt"))
    (check-equal? (part1 example-input) 7))

  (test-case "part2 example"
    (define example-input (parse-input "inputs/example.txt"))
    (check-equal? (part2 example-input) 33)))

(module+ main
  (define input (parse-input "day10/inputs/input.txt"))

  (define run
    (λ ()
      (printf "Part 1: ~a\n" (part1 input))
      (printf "Part 2: ~a\n" (part2 input))))

  (define prof? (getenv "PROFILE"))

  (if prof?
      (begin
        (printf "Profiling...\n")
        (profile-thunk run))
      (run)))
