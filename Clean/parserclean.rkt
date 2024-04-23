#lang racket

(require "ast.rkt")

(provide parse unparse)

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)              (Val s)]
    [(? boolean?)              (Val s)]
    [(? symbol?)               (Var s)]
    [(list (? unop? u) e)      (UnOp u (parse e))]
    [(list (? binop? b) e1 e2) (BinOp b (parse e1) (parse e2))]
    [`(if ,e1 ,e2 ,e3)         (If (parse e1) (parse e2) (parse e3))]
    [`(cond ,@cs (else ,e))    (Cond (parse-cond cs) (parse e))]
    [`(let ((,x ,e1)) ,e2)     (Let x (parse e1) (parse e2))]
    [`(lambda (,@xs) ,e)       (Lam xs (parse e))]
    [`(λ (,@xs) ,e)            (Lam xs (parse e))]
    [(cons e es)               (App (parse e) (map parse es))]
    [_                         (error "Parse error!")]))

(define (parse-cond cs)
  (match cs
    ['() '()]
    [(cons `(,pred ,e) rest) (cons (list (parse pred) (parse e))
                                (parse-cond rest))]))

;; Any -> Boolean
(define (unop? x)
  (memq x '(add1 sub1 zero?)))

;; Any -> Boolean
(define (binop? x)
  (memq x '(+ - * / <= and)))

(define (unparse e)
  (match e
    [(Val v) v]
    [(Var x) x]
    [(UnOp u e) (list u (unparse e))]
    [(BinOp b e1 e2) (list b (unparse e1) (unparse e2))]
    [(If e1 e2 e3) (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Cond cs e) (append '(cond) (unparse-cond cs) (list (list 'else (unparse e))))]
    [(Let x e1 e2) (list 'let (list (list x (unparse e1))) (unparse e2))]
    [(Lam xs e) (list 'λ xs (unparse e))]
    [(App ef es) (cons (unparse ef) (map unparse es))]))

(define (unparse-cond cs)
  (map (λ (c) (match c
                [(list pred e) (list (unparse pred) (unparse e))])) cs))
