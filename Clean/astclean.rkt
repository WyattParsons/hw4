#lang racket

(provide Val UnOp BinOp If Cond Err Err?
         Let Var App Lam)

; type Values :=
;   | (Val v)
;   | (Lam xs e)
(struct Val   (v)        #:prefab)
(struct Lam   (xs e)     #:prefab)

; type Expr :=
;   | Values
;   | (Var   x)
;   | (UnOp  u e)
;   | (BinOp u e)
;   | (If    e e e)
;   | (Let   x e e)
;   | (App   e e)
(struct Var   (x)        #:prefab)
(struct UnOp  (u e)      #:prefab)
(struct BinOp (b e1 e2)  #:prefab)
(struct If    (e1 e2 e3) #:prefab)
(struct Cond  (cs e)     #:prefab)
(struct Let   (x e1 e2)  #:prefab)
(struct App   (e es)   #:prefab)

(struct Err   (err)      #:prefab)
