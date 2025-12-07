#lang racket

(require math/array
         racket/match
         rackunit)

(provide (struct-out padded2d)
         padded2d-inner-shape
         padded2d-inner-slice-spec
         padded2d-inner-array
         pad-2d
         pad-n
         in-padded2d-inner-indexes
         padded2d-map-inner/index
         padded2d-map-inner
         padded2d-fold-inner/index
         padded2d-fold-inner
         neighbor-offsets/4
         neighbor-offsets/8
         padded2d-neighbor
         padded2d-neighbor-count-8
         string->char-array2d
         string->int-array2d
         string->int-array2d/mapping
         char-array2d->string
         check-grid-equal?
         integer->digit
         digit->integer
         overlay-where)

(struct padded2d (data pad-top pad-bottom pad-left pad-right) #:transparent)

;; Helper: extract all dimension info at once
(define (padded2d-dims padded)
  (match-define (padded2d data pad-top pad-bottom pad-left pad-right) padded)
  (match-define (vector total-rows total-cols) (array-shape data))
  (values total-rows total-cols pad-top pad-bottom pad-left pad-right))

;; Helper: convert inner index to outer index
(define (inner->outer-index inner-idx pad-top pad-left)
  (match-define (vector inner-row inner-col) inner-idx)
  (vector (+ pad-top inner-row) (+ pad-left inner-col)))

;; Helper: get cell at inner index
(define (padded2d-inner-ref padded inner-idx)
  (array-ref (padded2d-data padded)
             (inner->outer-index inner-idx (padded2d-pad-top padded) (padded2d-pad-left padded))))

(define (padded2d-inner-shape padded)
  (define-values (total-rows total-cols pad-top pad-bottom pad-left pad-right) (padded2d-dims padded))
  (define inner-rows (- total-rows pad-top pad-bottom))
  (define inner-cols (- total-cols pad-left pad-right))
  (when (or (negative? inner-rows) (negative? inner-cols))
    (error 'padded2d-inner-shape "padding too large for shape ~a" (vector total-rows total-cols)))
  (vector inner-rows inner-cols))

(define (padded2d-inner-slice-spec padded)
  (define-values (total-rows total-cols pad-top pad-bottom pad-left pad-right) (padded2d-dims padded))
  (list (:: pad-top (- total-rows pad-bottom)) (:: pad-left (- total-cols pad-right))))

(define (padded2d-inner-array padded)
  (array-slice-ref (padded2d-data padded) (padded2d-inner-slice-spec padded)))

(define (pad-2d arr pad-top pad-bottom pad-left pad-right [pad-val 0])
  (match-define (vector rows cols) (array-shape arr))
  (define new-rows (+ pad-top rows pad-bottom))
  (define new-cols (+ pad-left cols pad-right))
  (define padded
    (build-array
     (vector new-rows new-cols)
     (λ (index)
       (match-define (vector row col) index)
       (define inner-row (- row pad-top))
       (define inner-col (- col pad-left))
       (if (and (<= 0 inner-row) (< inner-row rows)
                (<= 0 inner-col) (< inner-col cols))
           (array-ref arr (vector inner-row inner-col))
           pad-val))))
  (padded2d padded pad-top pad-bottom pad-left pad-right))

(define (pad-n arr n [pad-val 0])
  (pad-2d arr n n n n pad-val))

(define (in-padded2d-inner-indexes padded)
  (in-array-indexes (padded2d-inner-shape padded)))

(define (padded2d-map-inner/index padded transform)
  (array-map (λ (inner-idx) (transform inner-idx (padded2d-inner-ref padded inner-idx)))
             (indexes-array (padded2d-inner-shape padded))))

(define (padded2d-map-inner padded transform)
  (padded2d-map-inner/index padded (λ (_inner-index cell) (transform cell))))

(define (padded2d-fold-inner/index padded combine init)
  (array-fold (λ (inner-idx acc) (combine inner-idx (padded2d-inner-ref padded inner-idx) acc))
              init
              (indexes-array (padded2d-inner-shape padded))))

(define (padded2d-fold-inner padded combine init)
  (padded2d-fold-inner/index padded (λ (_inner-index cell acc) (combine cell acc)) init))

(define neighbor-offsets/4 '((-1 0) (0 -1) (0 1) (1 0)))
(define neighbor-offsets/8 '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))

(define (padded2d-neighbor padded
                           offsets
                           combine-proc
                           #:cell-pred [cell-pred (λ (_) #t)]
                           #:default-val [default-val 0])
  (let* ([data (padded2d-data padded)]
         [pad-top (padded2d-pad-top padded)]
         [pad-bottom (padded2d-pad-bottom padded)]
         [pad-left (padded2d-pad-left padded)]
         [pad-right (padded2d-pad-right padded)])
  (match-define (vector inner-rows inner-cols) (padded2d-inner-shape padded))

    ;; Calculate required padding from offsets
    (define max-top (apply max (map (match-lambda [(list drow _dcol) (- drow)]) offsets)))
    (define max-bottom (apply max (map (match-lambda [(list drow _dcol) drow]) offsets)))
    (define max-left (apply max (map (match-lambda [(list _drow dcol) (- dcol)]) offsets)))
    (define max-right (apply max (map (match-lambda [(list _drow dcol) dcol]) offsets)))

    (when (or (< pad-top max-top)
              (< pad-bottom max-bottom)
              (< pad-left max-left)
              (< pad-right max-right))
      (error 'padded2d-neighbor
             "requires at least ~a padding (top bottom left right), got ~a"
             (list max-top max-bottom max-left max-right)
             (list pad-top pad-bottom pad-left pad-right)))

    (define (make-axis-slice pad inner-len delta)
      (let ([start (+ pad delta)]) (:: start (+ start inner-len))))

    (define slice-specs
      (map (match-lambda
             [(list drow dcol)
              (list (make-axis-slice pad-top inner-rows drow)
                    (make-axis-slice pad-left inner-cols dcol))])
           offsets))

    (define neighbor-layers (map (λ (spec) (array-slice-ref data spec)) slice-specs))

    (define inner-arr (padded2d-inner-array padded))
    (define neighbor-result (apply array-map combine-proc neighbor-layers))

    (array-map (λ (cell neighbor-val) (if (cell-pred cell) neighbor-val default-val))
               inner-arr
               neighbor-result)))

(define (padded2d-neighbor-count-8 padded)
  (padded2d-neighbor padded neighbor-offsets/8 +))

(define (integer->digit n)
  (if (and (>= n 0) (<= n 9))
      (integer->char (+ n (char->integer #\0)))
      (error 'integer->digit "expected single digit 0-9, got ~a" n)))

(define (digit->integer c)
  (if (char-numeric? c)
      (- (char->integer c) (char->integer #\0))
      (error 'digit->integer "expected digit character, got ~a" c)))

(define (overlay-where base-arr mask-arr overlay-val [mask-test (λ (v) (= v 1))])
  (define base-shape (array-shape base-arr))
  (define mask-shape (array-shape mask-arr))
  (unless (equal? base-shape mask-shape)
    (error 'overlay-where "shape mismatch: base ~a, mask ~a" base-shape mask-shape))
  (array-map (λ (base mask) (if (mask-test mask) overlay-val base)) base-arr mask-arr))

(define (strip-leading-newline s)
  (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\newline))
      (substring s 1)
      s))

(define (strip-common-indent s)
  (define lines (string-split s "\n"))
  (define non-empty-lines (filter (λ (line) (> (string-length (string-trim line)) 0)) lines))
  (if (null? non-empty-lines)
      s
      (let* ([indents (map (λ (line)
                             (- (string-length line)
                                (string-length (string-trim line #:left? #t #:right? #f))))
                           non-empty-lines)]
             [min-indent (apply min indents)])
        (string-join (map (λ (line)
                            (if (< (string-length line) min-indent)
                                line
                                (substring line min-indent)))
                          lines)
                     "\n"))))

(define (normalize-grid-lines s)
  (define lines (string-split s "\n"))
  (match (reverse lines)
    ['() '()]
    [(cons head tail)
     (if (and (zero? (string-length head)) (pair? tail))
         (reverse tail)
         lines)]))

(define (string->char-array2d s)
  (define stripped (strip-leading-newline s))
  (define dedented (strip-common-indent stripped))
  (define lines (normalize-grid-lines dedented))
  (when (null? lines)
    (error 'string->char-array2d "empty grid string"))
  (define width (string-length (first lines)))
  (unless (andmap (λ (line) (= (string-length line) width)) lines)
    (error 'string->char-array2d "grid is not rectangular: ~v" lines))
  (define height (length lines))
  (define line-vec (list->vector lines))
  (build-array (vector height width)
               (λ (index)
                 (match-define (vector row col) index)
                 (string-ref (vector-ref line-vec row) col))))

(define (string->int-array2d/mapping s char->int)
  (define char-arr (string->char-array2d s))
  (array-map (λ (c)
               (define result (char->int c))
               (unless (integer? result)
                 (error 'string->int-array2d/mapping
                        "mapping for character ~v returned non-integer: ~v"
                        c
                        result))
               result)
             char-arr))

(define (string->int-array2d s)
  (string->int-array2d/mapping s digit->integer))

(define (char-array2d->string arr #:elem->char [elem->char #f])
  (match-define (vector rows cols) (array-shape arr))
  (define convert
    (if elem->char
        elem->char
        (λ (v)
          (cond
            [(char? v) v]
            [else (error 'char-array2d->string "element ~v is not a char; supply #:elem->char" v)]))))
  (define lines
    (for/list ([row (in-range rows)])
      (list->string (for/list ([col (in-range cols)])
                      (convert (array-ref arr (vector row col)))))))
  (string-join lines "\n"))

(define (grid-diff-message expected-str actual-str)
  (define expected-lines (normalize-grid-lines expected-str))
  (define actual-lines (normalize-grid-lines actual-str))

  (define expected-rows (length expected-lines))
  (define actual-rows (length actual-lines))

  (define expected-cols
    (if (zero? expected-rows)
        0
        (string-length (first expected-lines))))
  (define actual-cols
    (if (zero? actual-rows)
        0
        (string-length (first actual-lines))))

  ;; Add shape info if dimensions differ
  (define shape-note
    (if (or (not (= expected-rows actual-rows)) (not (= expected-cols actual-cols)))
        (format "expected shape: ~ax~a~nactual   shape: ~ax~a~n~n"
                expected-rows
                expected-cols
                actual-rows
                actual-cols)
        ""))

  (format "~aexpected:~n~a~n~nactual:~n~a"
          shape-note
          (string-join expected-lines "\n")
          (string-join actual-lines "\n")))

;; Core comparison function - compares two grid strings
(define-check (check-grid-equal?/helper actual-str expected-str message)
  (define expected-stripped (strip-leading-newline expected-str))
  (define expected-dedented (strip-common-indent expected-stripped))
  (define expected-norm (string-join (normalize-grid-lines expected-dedented) "\n"))
  (define actual-stripped (strip-leading-newline actual-str))
  (define actual-dedented (strip-common-indent actual-stripped))
  (define actual-norm (string-join (normalize-grid-lines actual-dedented) "\n"))
  (unless (string=? expected-norm actual-norm)
    (define diff (grid-diff-message expected-norm actual-norm))
    (with-check-info (['message message] ['diff diff]) (fail-check (format "~n~a" diff)))))

(define (check-grid-equal? actual-str expected-str [message "grid did not match"])
  (check-grid-equal?/helper actual-str expected-str message))

(module+ test
  (define g (array #[#[1 2 3] #[4 5 6]]))

  (define p1 (pad-n g 1))
  (check-equal? (array-shape (padded2d-data p1)) #(4 5))
  (check-equal? (padded2d-inner-shape p1) #(2 3))
  (check-equal? (array-ref (padded2d-data p1) #(0 0)) 0)
  (check-equal? (array-ref (padded2d-data p1) #(1 1)) 1)
  (check-equal? (array-ref (padded2d-data p1) #(2 3)) 6)

  (define p2 (pad-n g 2 -1))
  (check-equal? (array-shape (padded2d-data p2)) #(6 7))
  (check-equal? (array-ref (padded2d-data p2) #(2 2)) 1)

  (let ([result (string->char-array2d "
   abc
   DEF
   ")])
    (check-grid-equal? (char-array2d->string result) "
   abc
   DEF
   "))

  (check-exn exn:fail?
             (λ ()
               (let ([result (string->char-array2d "
      abc
      DEF
      ")])
                 (check-grid-equal? (char-array2d->string result) "
      abc
      XEF
      "))))

  (let* ([int-arr (string->int-array2d "
   101
   010
   101
   ")]
         [padded (pad-n int-arr 1 0)]
         [result (padded2d-neighbor-count-8 padded)])
    (check-grid-equal? (char-array2d->string result #:elem->char integer->digit)
                       "
   131
   343
   131
   "))

  ;; Test that padded2d-neighbor-count-8 does not count the center, only neighbors
  ;; All cells are 1, so center cell should count 8 neighbors (not 9 if it counted itself)
  (let* ([int-arr (string->int-array2d "
   111
   111
   111
   ")]
         [padded (pad-n int-arr 1 0)]
         [result (padded2d-neighbor-count-8 padded)])
    (check-grid-equal? (char-array2d->string result #:elem->char integer->digit)
                       "
   353
   585
   353
   "))

  ;; Test generic padded2d-neighbor with 4-way neighbors
  (let* ([int-arr (string->int-array2d "
   111
   111
   111
   ")]
         [padded (pad-n int-arr 1 0)]
         [result (padded2d-neighbor padded neighbor-offsets/4 +)])
    (check-grid-equal? (char-array2d->string result #:elem->char integer->digit)
                       "
   232
   343
   232
   "))

  ;; Test padded2d-neighbor with max operation
  (let* ([int-arr (string->int-array2d "
   135
   246
   789
   ")]
         [padded (pad-n int-arr 1 0)]
         [result (padded2d-neighbor padded neighbor-offsets/8 max)])
    (check-grid-equal? (char-array2d->string result #:elem->char integer->digit)
                       "
   466
   899
   898
   "))

  (let* ([base (string->char-array2d "
  @.@
  .@.
  @.@
  ")]
         [mask (array #[#[0 1 0] #[1 1 1] #[0 1 0]])]
         [overlaid (overlay-where base mask #\x)])
    (check-grid-equal? (char-array2d->string overlaid) "
   @x@
   xxx
   @x@
   "))

  (displayln "✓ grid-helpers tests passed"))
