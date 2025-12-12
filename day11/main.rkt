#lang racket

(require profile
         graph)

(define (parse-input path)
  (unweighted-graph/adj (call-with-input-file path
                                              (λ (in)
                                                (for/list ([line (in-lines in)])
                                                  (map string->symbol
                                                       (regexp-match* #px"[a-z]+" line)))))))

(define (count-paths g start goal)
  (define memo (make-hasheq))

  (let count-from ([from start])
    (hash-ref! memo
               from
               (thunk (if (equal? from goal)
                          1
                          (for/sum ([to (in-neighbors g from)]) (count-from to)))))))

(define colors (hasheq 'fft 1 'dac 1 'svr 2 'you 3 'out 4))

(define (write-dot g path)
  (call-with-output-file
   path
   (λ (out) (graphviz g #:output out #:colors colors #:graph-attributes '((rankdir LR))))
   #:exists 'replace))

(define (part1 graph)
  (unless (dag? graph)
    (error 'part1 "expected a DAG"))
  (count-paths graph 'you 'out))

(define (make-rank-map g)
  (for/hasheq ([node (in-list (tsort g))]
               [i (in-naturals)])
    (values node i)))

(define (count-paths/route g route)
  (for/product ([from (in-list route)] [to (in-list (cdr route))]) (count-paths g from to)))

(define (feasible? ranks route)
  (for/and ([from (in-list route)]
            [to (in-list (cdr route))])
    (< (hash-ref ranks from) (hash-ref ranks to))))

(define (part2 graph)
  (unless (dag? graph)
    (error "Not a DAG"))

  (define ranks (make-rank-map graph))

  (define route-fft '(svr fft dac out))
  (define route-dac '(svr dac fft out))
  (define routes (filter (curry feasible? ranks) (list route-fft route-dac)))
  (apply + (map (curry count-paths/route graph) routes)))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example1.txt")))

  (check-equal? example1 5)
  (check-equal? example2 2))

(module+ main
  (define input (parse-input "day11/inputs/input.txt"))

  ;# (write-dot input "graph.dot")
  ;# (printf "Wrote graph.dot\n")

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
