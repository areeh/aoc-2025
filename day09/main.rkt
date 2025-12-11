#lang racket

(require math/array
         profile)

(define (lines->integer-array lines)
  (list*->array (for/list ([line (in-list lines)])
                  (map string->number (string-split line ",")))
                integer?))

(define (parse-input path)
  (lines->integer-array (file->lines path)))

(struct pt (x y) #:transparent)
(struct pair-dist (dist i j) #:transparent)
(struct h-edge (y x1 x2))
(struct v-edge (x y1 y2))

(define (vertices->point-vector vertices-array)
  (match-define (vector n _) (array-shape vertices-array))
  (build-vector
   n
   (λ (i) (pt (array-ref vertices-array (vector i 0)) (array-ref vertices-array (vector i 1))))))

(define (pairwise-rect-area coords)
  (match-define (vector n d) (array-shape coords))
  (unless (= d 2)
    (error 'pairwise-rect-area
           "expected coords with 2 dims (x,y), got shape ~a"
           (array-shape coords)))
  (let* ([a (array-reshape coords (vector n 1 d))]
         [b (array-reshape coords (vector 1 n d))]
         [diffs (array-map (λ (u v) (abs (- u v))) a b)]
         [diffs+1 (array+ diffs (array 1))]) ; inclusive
    (array-axis-prod diffs+1 2)))

(define (unique-pairs distances)
  (define n (vector-ref (array-shape distances) 0))
  (for*/list ([i (in-range n)]
              [j (in-range (add1 i) n)])
    (pair-dist (array-ref distances (vector i j)) i j)))

(define (sorted-pairs-by-area coords)
  (sort (unique-pairs (pairwise-rect-area coords)) > #:key pair-dist-dist))

(define (part1 input)
  (pair-dist-dist (first (sorted-pairs-by-area input))))

(define (point-on-segment? x y xi yi xj yj)
  (let ([cross (- (* (- xj xi) (- y yi)) (* (- yj yi) (- x xi)))])
    (and (= cross 0) (<= (min xi xj) x (max xi xj)) (<= (min yi yj) y (max yi yj)))))

(define (point-inside? point vertices-vec)
  (let* ([x (pt-x point)]
         [y (pt-y point)]
         [n (vector-length vertices-vec)]
         [vx (λ (i) (pt-x (vector-ref vertices-vec i)))]
         [vy (λ (i) (pt-y (vector-ref vertices-vec i)))])
    (define on-boundary?
      (for/or ([i (in-range n)])
        (define j (modulo (add1 i) n))
        (point-on-segment? x y (vx i) (vy i) (vx j) (vy j))))
    (if on-boundary?
        #t
        (for/fold ([inside? #f]) ([i (in-range n)])
          (define j (modulo (add1 i) n))
          (define yi (vy i))
          (define yj (vy j))
          (if (and (<= (min yi yj) y) (< y (max yi yj)))
              (let* ([xi (vx i)]
                     [xj (vx j)]
                     [x-int (+ xi (* (- xj xi) (/ (- y yi) (- yj yi))))])
                (if (> x-int x)
                    (not inside?)
                    inside?))
              inside?)))))

(define (vertices->sorted-hv-edges vertices-vec)
  (define n (vector-length vertices-vec))
  (define-values (h v)
    (for/fold ([h '()]
               [v '()])
              ([i (in-range n)])
      (let* ([j (modulo (add1 i) n)]
             [p1 (vector-ref vertices-vec i)]
             [p2 (vector-ref vertices-vec j)]
             [x1 (pt-x p1)]
             [y1 (pt-y p1)]
             [x2 (pt-x p2)]
             [y2 (pt-y p2)])
        (cond
          [(= y1 y2)
           (let* ([xa (min x1 x2)]
                  [xb (max x1 x2)])
             (values (cons (h-edge y1 xa xb) h) v))]
          [(= x1 x2)
           (let* ([ya (min y1 y2)]
                  [yb (max y1 y2)])
             (values h (cons (v-edge x1 ya yb) v)))]
          [else
           (error 'vertices->sorted-hv-edges "non axis-aligned edge (~a,~a)-(~a,~a)" x1 y1 x2 y2)]))))
  (values (list->vector (sort h < #:key h-edge-y)) (list->vector (sort v < #:key v-edge-x))))

(define (lower-bound vec key proj)
  (let loop ([lo 0]
             [hi (vector-length vec)])
    (if (>= lo hi)
        lo
        (let* ([mid (quotient (+ lo hi) 2)]
               [v (proj (vector-ref vec mid))])
          (if (< v key)
              (loop (add1 mid) hi)
              (loop lo mid))))))

(define (h/v-intersection? h v)
  (let* ([y (h-edge-y h)]
         [x1 (h-edge-x1 h)]
         [x2 (h-edge-x2 h)]
         [x (v-edge-x v)]
         [y1 (v-edge-y1 v)]
         [y2 (v-edge-y2 v)])
    (and (> x x1) (< x x2) (> y y1) (< y y2))))

(define (horizontal-rect-edge-ok? h vedge xa xb)
  (define start (lower-bound vedge xa v-edge-x))
  (let loop ([i start])
    (cond
      [(= i (vector-length vedge)) #t]
      [else
       (let* ([e (vector-ref vedge i)]
              [x (v-edge-x e)])
         (cond
           [(>= x xb) #t]
           [(h/v-intersection? h e) #f]
           [else (loop (add1 i))]))])))

(define (vertical-rect-edge-ok? v hedge ya yb)
  (define start (lower-bound hedge ya h-edge-y))
  (let loop ([i start])
    (cond
      [(= i (vector-length hedge)) #t]
      [else
       (let* ([e (vector-ref hedge i)]
              [y (h-edge-y e)])
         (cond
           [(>= y yb) #t]
           [(h/v-intersection? e v) #f]
           [else (loop (add1 i))]))])))

(define (part2 vertices-array)
  (define verts (vertices->point-vector vertices-array))
  (define sorted-pairs (sorted-pairs-by-area vertices-array))
  (define inside?
    (let ([memo (make-hash)])
      (λ (p)
        (define key (cons (pt-x p) (pt-y p)))
        (hash-ref! memo key (λ () (point-inside? p verts))))))
  (define-values (h-edges v-edges) (vertices->sorted-hv-edges verts))
  (define (valid-pair? pair)
    (let* ([p1 (vector-ref verts (pair-dist-i pair))]
           [p2 (vector-ref verts (pair-dist-j pair))]
           [x1 (pt-x p1)]
           [y1 (pt-y p1)]
           [x2 (pt-x p2)]
           [y2 (pt-y p2)]
           [xa (min x1 x2)]
           [xb (max x1 x2)]
           [ya (min y1 y2)]
           [yb (max y1 y2)]
           [corners (list (pt x1 y2) (pt x2 y1))]
           [rect-h (list (h-edge ya xa xb) (h-edge yb xa xb))]
           [rect-v (list (v-edge xa ya yb) (v-edge xb ya yb))])
      (and (for/and ([c (in-list corners)])
             (inside? c))
           (for/and ([h (in-list rect-h)])
             (horizontal-rect-edge-ok? h v-edges xa xb))
           (for/and ([v (in-list rect-v)])
             (vertical-rect-edge-ok? v h-edges ya yb)))))
  (for/first ([p (in-list sorted-pairs)]
              #:when (valid-pair? p))
    (pair-dist-dist p)))

(module+ test
  (require rackunit)

  (define example1 (part1 (parse-input "inputs/example.txt")))
  (define example2 (part2 (parse-input "inputs/example.txt")))

  (define coords (array #[#[0 0] #[1 0] #[0 1] #[1 1]]))
  (define expected (array #[#[1 2 2 4] #[2 1 4 2] #[2 4 1 2] #[4 2 2 1]]))

  (check-equal? (pairwise-rect-area coords) expected)
  (check-equal? example1 50)
  (check-equal? example2 24))

(module+ main
  (define input (parse-input "day09/inputs/input.txt"))

  (define run
    (λ ()
      (printf "Part 1: ~a\n" (part1 input))
      (printf "Part 2: ~a\n" (part2 input))))

  (define prof? (getenv "PROFILE"))

  (if prof?
      (begin
        (printf "Profiling part2...\n")
        (profile-thunk run))
      (run)))
