#lang racket

(provide cond->if currify)

(require "ast.rkt" "parser.rkt")

(define (cond->if e)
  ; TODO
  '())

(define (currify e)
  ; TODO
  '())

(module+ test
  (require rackunit)

  (check-equal? (unparse (cond->if (parse '(cond [(zero? (- 6 5)) 1]
                                                 [(<= 6 7)        2]
                                                 [else            3]))))
                '(if (zero? (- 6 5)) 1 (if (<= 6 7) 2 3)))

  (check-equal? (unparse (currify (parse '((λ (x y) (+ x y)) 2 3))))
                '(((λ (x) (λ (y) (+ x y))) 2) 3)))
