
#lang plai-typed

"(define (substituter [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
  (type-case λ-exp in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substituter what for l)
                        (substituter what for r)))
    (λ-def (v p)(λ-def v (substituter what for p)))
    )
  )

;; beta-transformer : ((λ x M) N) --> [M:x=N]
;; beta-transformer : λ-exp -> λ-exp

;; Purpose : λ-calculus beta-reduction naive implementation.

;; Template :
;(define (beta-transform (le : λ-exp)) : λ-exp
;  (type-case λ-exp le
;    (λ-sym (v) ...)
;    (λ-app (l r)
;                  ... l
;                  ... r
;    ))

(define (beta-transformer (le : λ-exp)) : λ-exp
  (type-case λ-exp le
    (λ-sym (v) le) ;; or (λ-sym v)
    (λ-app (l r) (if (λ-def? l)
                     (substituter r (λ-def-v l) (λ-def-p l))
                     (λ-app (beta-transformer l) (beta-transformer r))))
    (λ-def (v p) (λ-def v (beta-transformer p)))))" 


"Project7"

;; λ-expression grammar
;; λ-exp -> v
;; λ-exp -> (λ-exp λ-exp)
;; λ-exp -> (λ v λ-exp)
;; where v is a symbol.

;; λ-exp is an abstract syntax grammar or a parse tree definition for
;; - λ-exp that defined above.
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;; Tests:
(λ-sym 'x)
(λ-app (λ-sym 'x)(λ-sym 'y))
(λ-def 'v (λ-app (λ-sym 'x)(λ-sym 'y)))

;; parse : s-exp -> λ-exp
;; Purpose : To transform given s-expression to corresponding
(define (parserr (sexp : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? sexp)(λ-sym (s-exp->symbol sexp))]
    [(s-exp-list? sexp)
     (let ([sexp-list (s-exp->list sexp)])
       (cond
         [(= 2 (length sexp-list))
          (λ-app (parserr (first sexp-list))(parserr (second sexp-list)))]
         [(= 3 (length sexp-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first sexp-list)))
                   (s-exp-symbol? (second sexp-list)))
              (λ-def (s-exp->symbol(second sexp-list))
                     (parserr (third sexp-list)))
              (error 'parserr "Not valid λ-definition")
              )]
         [else (error 'parserr "Not valid length λ-exp")]
         ))]
    [else (error 'parserr "Not valid λ-exp")]
    ))

;; Tests:
(test (parserr (symbol->s-exp 'y))(λ-sym 'y))
(test (parserr '(λ x x))(λ-def 'x (λ-sym 'x)))
(test (parserr '((λ x x) y))(λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
(test (parserr '((λ x x)(λ y y)))
      (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
(test (parserr '(λ x (λ y (y x))))
      (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))


;; unparse : λ-exp -> s-exp
;; Purpose : To produce concrete syntax from given abstract syntax.
(define (unparser (le : λ-exp)) : s-expression
  (type-case λ-exp le
    (λ-sym (v) (symbol->s-exp v))
    (λ-app (l r)(list->s-exp (list (unparser l)(unparser r))))
    (λ-def (v p)(list->s-exp 
                 (list (symbol->s-exp 'λ)(symbol->s-exp v)(unparser p))))
    ))

;; Test:
(test (unparser (λ-sym 'y))(symbol->s-exp 'y))
(test (unparser (λ-def 'x (λ-sym 'x))) '(λ x x))
(test (unparser (λ-app (λ-def 'x (λ-sym 'x)) (λ-sym 'y)))
      '((λ x x) y))
(test (unparser (λ-app (λ-def 'x (λ-sym 'x))(λ-def 'y (λ-sym 'y))))
      '((λ x x)(λ y y)))
(test (unparser (λ-def 'x (λ-def 'y (λ-app (λ-sym 'y) (λ-sym 'x)))))
      '(λ x (λ y (y x))))

;; =========================================================== ;;

;; A set represented as a list.
;; set-union : (listof symbol) (listof symbol) -> (listof symbol)
;; Purpose : To find the union of two sets.
(define (set-union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; Tests:
(test (set-union empty empty) empty)
(test (set-union empty (list 'x)) (list 'x))
(test (set-union (list 'x)(list 'x 'y)) (list 'x 'y))


;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; Purpose : To find the set difference of two sets.
(define (set-difference (s1 : (listof symbol))  (s2 : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x s2)))
          s1))

;; Tests:
(test (set-difference empty (list 'x)) empty)
(test (set-difference (list 'x) empty) (list 'x))
(test (set-difference (list 'x)(list 'x 'y)) empty)
(test (set-difference (list 'x 'y)(list 'x))(list 'y))

;; free-identifier : λ-exp -> (listof symbol)
;; Purpose : To find free identifiers in given λ expression.
(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(set-union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(set-difference (free-identifier p)
                                (list v)))
    ))

"example"
(free-identifier (parserr (symbol->s-exp 'x))) 
(free-identifier (parserr '(λ x x))) 
(free-identifier (parserr '(λ x y))) 
(free-identifier (parserr '((λ x y)(λ y z)))) 
(free-identifier (parserr '((λ f y)(λ z z)))) 
(free-identifier (parserr '(λ x (λ y (y x))))) 
(free-identifier (parserr '(λ x (λ y z)))) 

;; Tests:
(test (free-identifier (parserr (symbol->s-exp 'x))) (list 'x))
(test (free-identifier (parserr '(λ x x))) empty)
(test (free-identifier (parserr '(λ x y))) (list 'y))
(test (free-identifier (parserr '((λ x y)(λ y z)))) (list 'y 'z))
(test (free-identifier (parserr '((λ f y)(λ z z)))) (list 'y))
(test (free-identifier (parserr '(λ x (λ y (y x))))) empty)
(test (free-identifier (parserr '(λ x (λ y z)))) (list 'z))





;; substituter : λ-exp  symbol  λ-exp -> λ-exp

;; Purpose : Substitution is the act of replacing a name 
;; - (in this case, that of the formal parameter) in an expression 
;; - (in this case, the body of the function) with another expression 
;; - (in this case, the actual parameter). [Directly from book.]

;; Template:
;; (define 
;; (substituter [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp  
;; <subst-body>
;;)

(define (substituter [what : λ-exp] [for : symbol] [in : λ-exp]) : λ-exp 
  (type-case λ-exp in
    (λ-sym (v) (if (symbol=? v for) 
                   what
                   in))
    (λ-app (l r) (λ-app (substituter what for l)
                        (substituter what for r)))
    (λ-def (v p)(λ-def v (substituter what for p)))
    )
  )
    

;; Purpose : λ-calculus beta-reduction naive implementation.
;; Template :
;(define (beta-transform (le : λ-exp)) : λ-exp
;  (type-case λ-exp le
;    (λ-sym (v) ...)
;    (λ-app (l r) ... l ... r)
;    (λ-def (v p) ...v ...p)
;    ))
(define (beta-transformer (le : λ-exp)) : λ-exp
  (type-case λ-exp le
    (λ-sym (v) le) ;; or (λ-sym v)
    (λ-app (l r) (if (λ-def? l)
                     (substituter r (λ-def-v l) (λ-def-p l))
                     (λ-app (beta-transformer l) (beta-transformer r))))
    (λ-def (v p) (λ-def v (beta-transformer p)))))


;; General Tests and Examples:
(define SQUARER
  (parserr '(λ f (λ x (f (f x))))))

(define CUBER
  (parserr '(λ f (λ x (f (f (f x)))))))

"EXAMPLE"

(beta-transformer (parserr '((λ x x) a)))
      

(beta-transformer (parserr '((λ x y) a)))
      

(beta-transformer (parserr '((λ x (a b)) k)))
      

(beta-transformer (parserr '((λ x (λ x y)) k)))
      

(beta-transformer (parserr '((λ x (λ y x)) k)))
      

(beta-transformer (parserr '((λ x (λ y (x y))) k)))
      






;Tests

(test (beta-transformer (parserr '((λ x x) a)))
      (parserr (symbol->s-exp 'a)))

(test (beta-transformer (parserr '((λ x y) a)))
      (parserr (symbol->s-exp 'y)))

(test (beta-transformer (parserr '((λ x (a b)) k)))
      (parserr '(a b)))

(test (beta-transformer (parserr '((λ x (λ x y)) k)))
      (parserr '(λ x y)))

(test (beta-transformer (parserr '((λ x (λ y x)) k)))
      (parserr '(λ y k)))

(test (beta-transformer (parserr '((λ x (λ y (x y))) k)))
      (parserr '(λ y (k y))))





