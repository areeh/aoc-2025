#lang racket

(provide string->lines
         process-paragraphs)

(define (string->lines s)
  (regexp-split #px"\r\n?|\n" s))

(define (normalize-newlines s)
  ;; \r\n -> \n,  \r -> \n
  (regexp-replace* #px"\r\n?" s "\n"))

(define (split-paragraphs s)
  ;; paragraphs separated by one or more blank lines
  (regexp-split #rx"\n[ \t]*\n" (normalize-newlines s)))

(define (process-paragraphs text . fs)
  (define ps (split-paragraphs text))
  (unless (= (length ps) (length fs))
    (error 'process-paragraphs "expected ~a paragraphs, got ~a" (length fs) (length ps)))
  ;; map is variadic: (map f xs ys) zips
  (map (λ (p f) (f p)) ps fs))
