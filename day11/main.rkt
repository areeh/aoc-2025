#lang racket

(require profile
         graph)

(define (parse-input path)
  (unweighted-graph/adj (call-with-input-file path
                                              (lambda (in)
                                                (for/list ([line (in-lines in)])
                                                  (map string->symbol
                                                       (regexp-match* #px"[a-z]+" line)))))))

(define (all-paths g start goal)
  (define memo (make-hasheq))

  (let paths-from ([from start])
    (hash-ref! memo
               from
               (thunk (if (equal? from goal)
                          (list (list goal))
                          (for*/list ([to (in-neighbors g from)]
                                      [path (in-list (paths-from to))])
                            (cons from path)))))))

(define colors (hasheq 'fft 1 'dac 1 'svr 2 'you 3 'out 4))

(define (write-dot g path)
  (call-with-output-file
   path
   (λ (out) (graphviz g #:output out #:colors colors #:graph-attributes '((rankdir LR))))
   #:exists 'replace))

(define (part1 graph)
  ; (displayln (graphviz graph))
  (unless (dag? graph)
    (error 'part1 "expected a DAG"))
  (define paths (all-paths graph 'you 'out))
  (length paths))

(define (has-path? g start goal)
  (do-dfs g
          #:order (lambda (_all) (list start))
          #:init #f
          #:prologue (_from to acc)
          (or acc (equal? to goal))
          #:break (_from _to acc)
          acc
          #:return (acc)
          acc))

(define (has-path?/route graph route)
  (for/and ([from (in-list route)]
            [to (in-list (cdr route))])
    (has-path? graph from to)))

(define (count-paths/route graph route)
  (for/product ([from (in-list route)] [to (in-list (cdr route))])
               (length (all-paths graph from to))))

(define (part2 graph)
  (unless (dag? graph)
    (error 'part2 "expected a DAG"))

  (define route-fft '(svr fft dac out))
  (define route-dac '(svr dac fft out))

  (define routes (filter (curry has-path?/route graph) (list route-fft route-dac)))
  (apply + (map (curry count-paths/route graph) routes)))

(module+ test
  (require rackunit)
  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example1.txt")))

  (check-equal? example1 5)
  (check-equal? example2 2))

(module+ main
  (define input (parse-input "day11/inputs/input.txt"))

  (write-dot input "graph.dot")
  (printf "Wrote graph.dot\n")

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
