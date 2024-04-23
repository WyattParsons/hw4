;in rewriter.rkt
#lang racket

(provide cond->if
         currify)

(require "ast.rkt"
         "parser.rkt")

(define (cond->if e)
  (displayln (format "Transforming: ~a" e))
  (match e
    [(Cond cs else) 
     (display "Handling Cond: ")
     (cond->if-helper cs else)]
    [(If cond true-branch false-branch)
     (displayln "Handling If: ")
     (If (cond->if cond) (cond->if true-branch) (cond->if false-branch))]
    [(App f args)
     (displayln "Handling Application: ")
     (App (cond->if f) (map cond->if args))]
    [(Lam x body)
     (displayln (format "Handling Lambda: variable ~a" x))
     (Lam x (cond->if body))]
    [(BinOp op a b)
     (displayln (format "Handling Binary Operation: operator ~a" op))
     (BinOp op (cond->if a) (cond->if b))]
    [(UnOp op a)
     (displayln (format "Handling Unary Operation: operator ~a" op))
     (UnOp op (cond->if a))]
    [else
     (displayln (format "Encountered basic expression or unknown type: ~a" e))
     e]))

(define (cond->if-helper cs else)
  (displayln (format "cond->if-helper: cases ~a, else ~a" cs else))
  (match cs
    ['()
     (displayln "No more conditions, handling else: ")
     (cond->if else)]
    [(cons `(,pred ,e) rest)
     (displayln (format "Handling condition with predicate: ~a" pred))
     (If (cond->if pred)
         (cond->if e)
         (cond->if-helper rest else))]))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (currify e)
  (displayln (format "Currifying expression: ~a" e))
  (match e
    [(UnOp op arg)
     (displayln (format "Handling Unary Operation: operator ~a, arg ~a" op arg))
     (UnOp op (currify arg))]
    [(BinOp op left right)
     (displayln (format "Handling Binary Operation: operator ~a, left ~a, right ~a" op left right))
     (BinOp op (currify left) (currify right))]
    [(If cond true-branch false-branch)
     (displayln (format "Handling If expression: condition ~a, true branch ~a, false branch ~a" cond true-branch false-branch))
     (If (currify cond) (currify true-branch) (currify false-branch))]
    [(Lam xs body)
     (displayln (format "Handling Lambda with parameters: ~a, body: ~a" xs body))
     (if (null? xs)
         (Lam '() (currify body)) 
         (currify-lam xs body))]
    [(App f args)
     (displayln (format "Handling Application: function ~a, args ~a" f args))
     (let ([curried-f (currify f)])
       (define curried-args (map currify args))
       (displayln (format "Curried function: ~a, Curried args: ~a" curried-f curried-args))
       (apply-curried curried-f curried-args))]
    [(Let x e1 e2)
     (displayln (format "Handling Let: binding ~a" x))
     (Let x (currify e1) (currify e2))]
    [_
     (displayln (format "Handling basic or unknown expression type: ~a" e))
     e]))

(define (currify-lam xs body)
  (displayln (format "currify-lam called with parameters: ~a, body: ~a" xs body))
  (if (null? xs)
      (currify body)
      (Lam (list (car xs)) (currify-lam (cdr xs) (currify body)))))

(define (apply-curried f args)
  (displayln (format "apply-curried called with function: ~a, args: ~a" f args))
  (if (null? args)
      f
      (apply-curried (App f (list (car args))) (cdr args))))

(module+ test
  (require rackunit)

  ; ;; Test 2 & 3: Currifying a lambda with no parameters (should remain unchanged)
  ; (check-equal? (unparse (currify (parse '(λ () 5)))) '(λ () 5))

  ; ;; Test 4 & 5: Currifying within a let expression with a lambda taking multiple parameters
  ; (check-equal? (unparse (currify (parse '(let ([div (λ (x y) (/ x y))]) (div 4 2)))))
  ;               '(let ([div (λ (x) (λ (y) (/ x y)))]) ((div 4) 2)))

  ; ;; Test 6: Currifying a lambda in a function application
  ; (check-equal? (unparse (currify (parse '(add1 ((λ (x y) (/ x y)) 4 2)))))
  ;               '(add1 (((λ (x) (λ (y) (/ x y))) 4) 2)))

  ; ;; Test 7: Currifying a lambda in an arithmetic operation
  ; (check-equal? (unparse (currify (parse '(+ 3 ((λ (x y) (/ x y)) 4 2)))))
  ;               '(+ 3 (((λ (x) (λ (y) (/ x y))) 4) 2)))

  ; ;; Test 8: Currifying a lambda in an if expression
  ; (check-equal? (unparse (currify (parse '(if #t 4 ((λ (x y) (/ x y)) 4 2)))))
  ;               '(if #t 4 (((λ (x) (λ (y) (/ x y))) 4) 2)))

  ; ;; Test 9: Currifying a complex nested lambda application
  ; (check-equal? (unparse (currify (parse '(((λ (x y) (λ (z) (+ z (/ x y)))) 4 2) 5))))
  ;               '((((λ (x) (λ (y) (λ (z) (+ z (/ x y))))) 4) 2) 5))

  ; ;; Test 10: Currifying multiple nested lambda applications
  ; (check-equal? (unparse (currify (parse '((λ (x y) (/ x y)) 4 ((λ (x y) (+ x y)) 2 3)))))
  ;               '(((λ (x) (λ (y) (/ x y))) 4) (((λ (x) (λ (y) (+ x y))) 2) 3)))
                

;   currify: Test 2
; --------------------
; currify: Test 2
; FAILURE
; name:       check-eval-equal?
; location:   source/utils.rkt:219:4
; params:     '((currify (parse '((λ () 5)))) (parse '((λ () 5))))
; --------------------

(check-equal? (unparse (currify (parse '(λ () 5)))) '(λ () 5))


; currify: Test 3
; --------------------
; currify: Test 3
; FAILURE
; name:       check-eval-equal?
; location:   source/utils.rkt:219:4
; params:     '((currify (parse '((λ () 5)))) (parse '((λ () 5))))
; --------------------
(check-equal? (unparse (currify (parse '(λ () 5)))) '(λ () 5))




)
