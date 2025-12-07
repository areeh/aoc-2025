#lang racket

(provide string->lines
         process-paragraphs)

(define (string->lines str)
  (regexp-split #px"\r\n?|\n" str))

(define (normalize-newlines str)
  ;; \r\n -> \n,  \r -> \n
  (regexp-replace* #px"\r\n?" str "\n"))

(define (split-paragraphs str)
  ;; paragraphs separated by one or more blank lines
  (regexp-split #rx"\n[ \t]*\n" (normalize-newlines str)))

(define (process-paragraphs text . fs)
  (define paras (split-paragraphs text))
  (unless (= (length paras) (length fs))
    (error 'process-paragraphs "expected ~a paragraphs, got ~a" (length fs) (length paras)))
  ;; map is variadic: (map f xs ys) zips
  (map (λ (para fn) (fn para)) paras fs))
