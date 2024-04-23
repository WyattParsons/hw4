#lang racket

(require "ast.rkt"
         "parser.rkt")

(provide interp-err)

;; interp-err :: Expr -> Val or Err
(define (interp-err e)
  (with-handlers ([Err? (λ (err) err)])
    (interp '() '() e)))

;; interp :: Defn -> Env -> Expr -> Val
(define (interp D E e)
  (match e
    [(Val v) v]
    [(Var x) (lookup D E x)]
    [(UnOp u e) (interp-unop D E u e)]
    [(BinOp b e1 e2) (interp-binop D E b e1 e2)]
    [(If e1 e2 e3) (interp-if D E e1 e2 e3)]
    [(Let x e1 e2) (interp D (store E x (interp D E e1)) e2)]
    [(Lam xs e) (interp-lam D E xs e)]
    [(App e es) (interp-app D E e es)]))

;; interp-lam :: Defn -> Env -> Vars -> Expr -> Val
(define (interp-lam D E xs body)
  (λ (aargs) (interp D (append (zip xs aargs) E) body)))

;; interp-app :: Defn -> Env -> Expr -> Exprs -> Val
(define (interp-app D E f es)
  (let ([fn (interp D E f)] [args (map (λ (arg) (interp D E arg)) es)]) (fn args)))

;; interp-unop :: Defn -> Env -> UnOp -> Val
(define (interp-unop D E u e)
  (match u
    ['add1
     (match (interp D E e)
       [(? integer? i) (add1 i)]
       [_ (raise (Err "add1 expects int"))])]
    ['sub1
     (match (interp D E e)
       [(? integer? i) (sub1 i)]
       [_ (raise (Err "sub1 expects int"))])]
    ['zero?
     (match (interp D E e)
       [0 #t]
       [_ #f])]))

;; interp-binop :: Defn -> Env -> BinOp -> Expr -> Expr -> Val
(define (interp-binop D E b e1 e2)
  (match b
    ['+
     (match* ((interp D E e1) (interp D E e2))
       [((? integer? i1) (? integer? i2)) (+ i1 i2)]
       [(_ _) (raise (Err "+ requires int"))])]

    ['-
     (match* ((interp D E e1) (interp D E e2))
       [((? integer? i1) (? integer? i2)) (- i1 i2)]
       [(_ _) (raise (Err "- requires int"))])]

    ['*
     (match* ((interp D E e1) (interp D E e2))
       [((? integer? i1) (? integer? i2)) (* i1 i2)]
       [(_ _) (raise (Err "* requires int"))])]

    ['/
     (match* ((interp D E e1) (interp D E e2))
       [((? integer? i1) (? integer? i2))
        (if (eq? i2 0) (raise (Err "division by 0 not allowed")) (quotient i1 i2))]
       [(_ _) (raise (Err "/ requires int"))])]

    ['<=
     (match* ((interp D E e1) (interp D E e2))
       [((? integer? i1) (? integer? i2)) (<= i1 i2)]
       [(_ _) (raise (Err "<= requires int"))])]

    ['and
     (match (interp D E e1)
       [#f #f]
       [? (interp D E e2)])]))

;; interp-if :: Defn -> Env -> Expr -> Expr -> Expr -> Val
(define (interp-if D E e1 e2 e3)
  (match (interp D E e1)
    [#f (interp D E e3)]
    [_ (interp D E e2)]))

(define zip (lambda (l1 l2) (map list l1 l2)))

;; store :: Env -> Symbol -> Val -> Env
(define (store E x v)
  (cons (list x v) E))

;; lookup :: Defn -> Env -> Symbol -> Val
(define (lookup D E x)
  ; lookup the environment first, then the list of definitions
  (match E
    ['() (raise (Err (string-append "Unbound identifier: " (symbol->string x))))]
    [(cons (list y val) rest) (if (eq? x y) val (lookup D rest x))]))
