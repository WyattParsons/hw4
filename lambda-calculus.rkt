;in lambda-calculus.rkt
#lang racket

(provide parse
         unparse
         free?
         alpha-reduce
         beta-reduce)

(struct Var (x) #:prefab)
(struct Lam (x e) #:prefab)
(struct App (f arg) #:prefab)

(define (parse s)
  (match s
    [(? symbol?) (Var s)]
    [`(λ (,x) ,e) (Lam x (parse e))]
    [`(lambda (,x) ,e) (Lam x (parse e))]
    [(list f arg) (App (parse f) (parse arg))]))

(define (unparse e)
  (match e
    [(Var x) x]
    [(Lam x e) (list 'λ (list x) (unparse e))]
    [(App f arg) (list (unparse f) (unparse arg))]))

; TODO
(define (free-helper e bound)
  (displayln (format "free-helper called with expr: ~a, current bound: ~a" e bound))
  (match e
    [(Var x)
     (displayln (format "Evaluating variable: ~a with bound list: ~a" x bound))
     (if (member x bound)
         (begin (displayln (format "Variable ~a is bound" x)) '())
         (begin (displayln (format "Variable ~a is free" x)) (list x)))]
    [(Lam x body)
     (begin
       (displayln (format "Entering lambda with var ~a, current bound list before update: ~a" x bound))
       (let ([new-bound (cons x bound)])
         (displayln (format "Updated bound list for body of lambda ~a: ~a" x new-bound))
         (free-helper body new-bound)))]
    [(App f arg)
     (begin
       (displayln (format "Processing application with function: ~a and argument: ~a, current bound list: ~a" f arg bound))
       (let* ([f-free (free-helper f bound)]
              [arg-free (free-helper arg bound)])  ; Ensure argument is evaluated with the same bound context
         (displayln (format "Free vars in function part: ~a, free vars in argument part: ~a, combined bound list: ~a" f-free arg-free bound))
         (remove-duplicates (append f-free arg-free) equal?)))]))

(define (free? bound id e)
  (displayln (format "Starting free? check for id: ~a in expression: ~a with initial bound list: ~a" id e bound))
  (let ([free-vars (free-helper e bound)])
    (displayln (format "Computed free variables: ~a, checking for id: ~a" free-vars id))
    (member id free-vars)))





; TODO
; z has to be fresh
(define (alpha-reduce M x z)
  (match M
    [(Var y) (if (equal? y x) (Var z) M)]
    [(Lam y body) (if (equal? y x) (Lam z (alpha-reduce body x z)) (Lam y (alpha-reduce body x z)))]
    [(App f arg) (App (alpha-reduce f x z) (alpha-reduce arg x z))]))

; TODO
(define (beta-reduce M x N)
  (match M
    [(Var y)
     (begin

       (if (equal? y x) N M))]
    [(Lam y body)
     (begin

       (if (equal? y x)
           (Lam y body) ; If the lambda's variable is the one being replaced, do nothing.
           (let ([free-in-N (free-vars N)])

             (if (member y free-in-N)
                 (begin

                   (let ([new-var (gensym y)])

                     (Lam new-var (beta-reduce (subst body y (Var new-var)) x N))))
                 (begin

                   (Lam y (beta-reduce body x N)))))))]
    [(App f arg)
     (begin

       (App (beta-reduce f x N) (beta-reduce arg x N)))]))

;! Helper
(define (free-vars e)
  (let ([result (match e
                  [(Var x)
                   (begin
                     (displayln (format "Variable encountered: ~a" x))
                     (list x))]
                  [(Lam x body)
                   (begin
                     (displayln (format "Lambda with bound variable: ~a" x))
                     (remove x (free-vars body)))]
                  [(App f arg)
                   (begin
                     (displayln "Processing application in free-vars")
                     (union (free-vars f) (free-vars arg)))])])
    (displayln (format "free-vars result for ~a: ~a" e result))
    result))


;! Helper
(define (union lst1 lst2)
  (remove-duplicates (append lst1 lst2)))

;! Helper
(define (subst e old new)
  (match e
    [(Var x)
     (begin
       (displayln (format "Checking variable ~a for substitution with ~a" x old))
       (if (equal? x old) new (Var x)))]
    [(Lam x body)
     (begin
       (displayln (format "Substituting in lambda ~a, checking if ~a is the same as ~a" x old x))
       (if (equal? x old) (Lam x body) (Lam x (subst body old new))))]
    [(App f arg)
     (begin
       (displayln (format "Substituting in application: ~a ~a" f arg))
       (App (subst f old new) (subst arg old new)))]))


; (module+ test
;   (require rackunit)

;   (define fresh-x (gensym "x"))

;   (define fresh-x-symbol (gensym "x"))

;   (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'x fresh-x-symbol))
;                 (list 'λ (list 'y) fresh-x-symbol))

;   (define fresh-y-symbol (gensym "y"))

;   (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'y fresh-y-symbol))
;                 (list 'λ (list fresh-y-symbol) 'x))

;   (check-equal? (free? '() 'x (parse '(x ((λ (x) x) y)))) '(x y))
;   (check-equal? (free? '(x) 'x (parse '(x ((λ (x) x) y)))) '(y))
;   (check-equal? (free? '() 'x (parse '(z ((λ (x) x) y)))) '(z y))

;   (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'x fresh-x)) '(λ (y) fresh-x))
;   ; (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'y (gensym "y")))
;              ;   '(λ (fresh-y) x)) ; fresh-y would be whatever gensym "y" produces.

;   (check-equal? (unparse (beta-reduce (parse '(x x)) 'x (parse '(λ (x) (x x)))))
;                 '((λ (x) (x x)) (λ (x) (x x))))
;   (check-match (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y))) `(λ (,(? symbol?)) y)))

(module+ test
  (require rackunit)

  (define fresh-x (gensym "x"))
  (define fresh-y (gensym "y"))

  ; ;((free? '() 'u (parse '((λ (x) ((z v) (x u))) ((λ (z) z) (u y))))) #t)
  ; (check-equal? (free? '() 'u (parse '((λ (x) ((z v) (x u))) ((λ (z) z) (u y))))) '(u y))

  ; ; ((free? '() 'x (parse '(x ((λ (x) x) y)))) #t)
  ; (check-equal? (free? '() 'x (parse '(x ((λ (x) x) y)))) '(x y))

  ; ((free? '() 'u (parse '((λ (x) ((z v) (x u))) ((λ (z) z) (u y))))) #t)
  ; (check-equal? (free? '() 'u (parse '((λ (x) ((z v) (x u))) ((λ (z) z) (u y))))) '(u y))

  ; (check-equal? (free? '(x) 'y (parse '(x ((λ (x) x) y))))
  ;               #t
  ;               "Check if 'y' is correctly identified as free when 'x' is bound")

  ; (check-equal? (free? '() 'x (parse '(z ((λ (x) x) y)))) '(z y))

  ; (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'x fresh-x))
  ;               (list 'λ (list 'y) (unparse (Var fresh-x))))

  ; (check-equal? (unparse (alpha-reduce (parse '(λ (y) x)) 'y fresh-y)) (list 'λ (list fresh-y) 'x))

  ; (check-equal? (unparse (beta-reduce (parse '(x x)) 'x (parse '(λ (x) (x x)))))
  ;               '((λ (x) (x x)) (λ (x) (x x))))

  ; (check-match (unparse (beta-reduce (parse '(λ (y) x)) 'x (parse 'y))) `(λ (,(? symbol?)) y))

  ; (check-equal? (free? '() 'x (parse '(x ((λ (y) (λ (x) y)) z)))) '(x z))

  ; (check-equal? (free? '() 'u (parse '(u ((λ (x) ((λ (z) x) u)) y)))) '(u y))

  ;   Test 1
  ; Expression: (x ((λ (x) x) y))
  ; Expected: x is free in the expression since the inner lambda shadows x.
  ; Your Test Should: Check if x is free outside the inner lambda.
  ; (check-equal? (free? '() 'x (parse '(x ((λ (x) x) y)))) '(x y))

  ; ; Test 2
  ; ; Expression: (x ((λ (x) x) y))
  ; ; Context: x is in the bound list.
  ; ; Expected: x should not be free since it's in the bound list, although it appears in the expression.
  ; ; Your Test Should: Check if x is free outside the inner lambda.
  ; (check-equal? (free? '(x) 'x (parse '(x ((λ (x) x) y)))) '(y))

  ; ;   Test 3
  ; ; Expression: (z ((λ (x) x) y))
  ; ; Expected: x is not free; it is bound by the inner lambda, but z should be free.
  ; (check-equal? (free? '() 'x (parse '(z ((λ (x) x) y)))) '(z y))
  ; Test 4
  ; Expression: ((λ (x) ((z v) (x u))) ((λ (z) z) (u y)))
  ; Expected: u appears free since it is used in the body of a lambda that does not bind u.
  ; (check-equal? (free? '() 'u (parse '(u ((λ (x) ((λ (z) z) u)) y)))) '(u y))
  ; Test 5
  ; Expression: ((λ (x) ((z v) (x u))) ((λ (z) z) (u y)))
  ; Expected: x should not be free, it's bound in the lambda.
  ; (check-equal? (free? '() 'x (parse '(u ((λ (x) ((λ (z) z) u)) y)))) '(u y))

  ; Check if 'y' is free in the expression
  (check-equal? (free? '() 'y (parse '(y ((λ (y) (λ (x) y)) x)))) '(y))

  ; ; Check if 'x' is free in the expression
  ; (check-equal? (free? '() 'x (parse '(x ((λ (x) x) y)))) '(x))

  ; ;! (check-equal? (unparse (alpha-reduce (parse '((λ (x) ((λ (z) x) x)) y)) 'x (gensym "new-x")))
  ; ;               '((λ (new-x) ((λ (z) new-x) new-x)) y))

  ; ;! (check-equal? (unparse (alpha-reduce (parse '(λ (x) (λ (y) (λ (z) (x y z))))) 'y (gensym "new-y")))
  ; ;               '(λ (x) (λ (new-y) (λ (z) (x new-y z)))))

  ; (check-equal? (unparse (beta-reduce (parse '(((λ (x) (λ (y) (x y))) a) b)) 'a (parse 'z)))
  ;               '(((λ (x) (λ (y) (x y))) z) b))
  )
