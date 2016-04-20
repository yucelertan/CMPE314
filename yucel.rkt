
#lang plai-typed

;yucel cicek
;112200026

"CLASSWORK5 TEST
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp ""shouldn't get here"")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst 
                           (numC (interp a fds)) ;; Make it eager evaluation !!
                           ;; a - if it is lazy !
                           (fdC-arg fd)
                           (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(test ( interp (parser(fundef fa (x y) (+ x (* 2 y)))  ;Proje 4 classwork kısmı hocam ama ExprC ve parse kodu olmadığı için çalışmayacaktır doğru olup olmadığını kontrol ederseniz çok sevinirim hocam.
(fa 2 x)))))

(test (interp (fundef fa (x y) (+ x (* 2 y))) 
(fa y 3))))


"

;define msl
;main function
;; msl -> num
;; msl -> msl+msl
;; msl -> msl*msl
;; msl -> (msl)
;; Alphabet: [+, *, (), -, num]

;; msl is a typed defined as follows,
;; msl = <num>
;; msl = (add <msl> <msl>)
;;        (sub <msl> <msl>)
;;       (mult <msl> <msl>)
;;       (div <msl> <msl>)
;;       

(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mult (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-div (lhs : msl) (rhs : msl)]
  )

;;Tests
(msl-num 5)
(msl-add (msl-num 6) (msl-num 4))
(msl-mult (msl-num 6) (msl-num 4))
(msl-add (msl-add (msl-num 2) (msl-num 5)) (msl-num 8))
(msl-mult (msl-add (msl-num 3) (msl-num 12)) (msl-num 5))



(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]   
    [msl-mult (lhs rhs) (* (eval lhs) (eval rhs))]   
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]   
    [msl-div (lhs rhs) (/ (eval lhs) (eval rhs))]  
    ))

;;test
;; (msl-num 7) -> 7
;; (msl-num 5) -> 5
;; (msl-add (msl-num 3) (msl-num 4)) -> 7
;; (msl-sub (msl-num 5) (msl-num 2)) -> 3
;;(msl-div (msl-num 8) (msl-num 1)) -> 8
;;(msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))-> 42
;;(msl-mult (msl-add (msl-num 3) (msl-num 4)) (msl-num 5)) ->35
;; (msl-add (msl-div (msl-num 20) (msl-num 5)) (msl-num 35)) -> 39


(test (eval (msl-num 7))  7)
(test (eval (msl-num 5))  5)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-sub (msl-num 5) (msl-num 2)))  3)
(test (eval (msl-div (msl-num 8) (msl-num 1)))  8)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (eval (msl-mult (msl-add (msl-num 3) (msl-num 4)) (msl-num 5)))  35)
(test (eval (msl-add (msl-div (msl-num 20) (msl-num 5)) (msl-num 35)))  39)


(define (parse [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse (second sl)) (parse (third sl)))]
         [(*) (msl-mult (parse (second sl)) (parse (third sl)))]
         [(-) (msl-sub (parse (second sl)) (parse (third sl)))]
         [(/) (msl-div (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;;testing parse
;; '7 -> (msl-num 7)
;; '2 -> (msl-num 2)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(/ 20 4) -> (msl-add (msl-num 20) (msl-num 4))
;; '(* 2 8) -> (msl-add (msl-num 2) (msl-num 8))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
;; '(+ (- 3 4) 35) -> (msl-add (msl-sub (msl-num 3) (msl-num 4)) (msl-num 35))

(test (parse '7) (msl-num 7))
(test (parse '2) (msl-num 2))
(test (parse '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse '(/ 20 4)) (msl-div (msl-num 20) (msl-num 4)))
(test (parse '(* 2 8)) (msl-mult (msl-num 2) (msl-num 8)))
(test (parse '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))
(test (parse '(+ (- 3 4) 35)) (msl-add (msl-sub (msl-num 3) (msl-num 4)) (msl-num 35)))

;;-----------------------------------------------------------------------------------------------------------------------------------------------------

;; PARSER FOR PREFIX
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))

(define (parse-prefix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(*) (msl-mult (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(-) (msl-sub (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [else (error 'parse-prefix "invalid list input")]))]
    [else (error 'parse-prefix "invalid input")]))


(test (parse-prefix '7) (msl-num 7))
(test (parse-prefix '2) (msl-num 2))
(test (parse-prefix '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-prefix '(- 20 4)) (msl-sub (msl-num 20) (msl-num 4)))
(test (parse-prefix '(* 2 8)) (msl-mult (msl-num 2) (msl-num 8)))
(test (parse-prefix '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))
(test (parse-prefix '(+ (- 3 1) 35)) (msl-add (msl-sub (msl-num 3) (msl-num 1)) (msl-num 35)))
(test (parse-prefix '(- (* 3 4) 6)) (msl-sub (msl-mult (msl-num 3) (msl-num 4)) (msl-num 6)))

;; PARSER FOR INFIX
;; examples
;; '2 -> (msl-num 2)
;; '(7 + 8) -> (msl-add (msl-num 7) (msl-num 8))
;; '(6 + 5) -> (msl-add (msl-num 6) (msl-num 5))
;; '(5 * (3 * 4)) (msl-mul (msl-num 5)(msl-mult (msl-num 3)(msl-num 4))))

(define (parse-infix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([s1 (s-exp->list s)])
       (case (s-exp->symbol (second s1))
         [(+) (msl-add (parse-infix (first s1)) (parse-infix (third s1)))]
         [(*) (msl-mult (parse-infix (first s1)) (parse-infix (third s1)))]
         [(-) (msl-sub (parse-infix (first s1)) (parse-infix (third s1)))]
         [else (error 'parse-infix  "invalid list input")]))]
    [else (error 'parse-infix "invalid input")]))

;;Tests
(test (parse-infix '2) (msl-num 2))
(test (parse-infix '5) (msl-num 5))
(test (parse-infix '6) (msl-num 6))
(test (parse-infix '7) (msl-num 7))
(test (parse-infix '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-infix '(2 - 5)) (msl-sub (msl-num 2) (msl-num 5)))
(test (parse-infix '(6 * 7)) (msl-mult (msl-num 6) (msl-num 7)))
(test (parse-infix '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-infix '(2 - (3 * 4))) (msl-sub (msl-num 2) (msl-mult (msl-num 3)(msl-num 4))))
(test (parse-infix '((5 * 5) + 10)) (msl-add (msl-mult (msl-num 5)(msl-num 5))(msl-num 10)))
(test (parse-infix '((15 * 4) - 10)) (msl-sub (msl-mult (msl-num 15)(msl-num 4))(msl-num 10)))
(test (parse-infix '((10 + 15) + (20 * 2))) (msl-add (msl-add (msl-num 10)(msl-num 15))(msl-mult (msl-num 20)(msl-num 2))))
(test (parse-infix '((12 + 8) - (5 + 6))) (msl-sub (msl-add (msl-num 12)(msl-num 8))(msl-add (msl-num 5)(msl-num 6))))

;; template
;;(define (output-reverse-polish [expr : msl])
;; (type-case msl expr
;; [msl-num (n) ..]
;; [msl-add (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-mult (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-sub (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-exp (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]



(define (output-reverse-polish [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
    [msl-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '+)))]
    [msl-mult (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '*)))]
    [msl-sub (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '-)))]
    [msl-div (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '/)))]
    ))

(test (output-reverse-polish (msl-num 4)) (s-exp->list '(4)))
(test (output-reverse-polish (msl-num 5)) (s-exp->list '(5)))
(test (output-reverse-polish (msl-add (msl-num 5) (msl-num 7))) (s-exp->list '(5 7 +)))
(test (output-reverse-polish (msl-add (msl-num 5) (msl-num 6))) (s-exp->list '(5 6 +)))
(test (output-reverse-polish (msl-mult (msl-num 10) (msl-num 15))) (s-exp->list '(10 15 *)))
(test (output-reverse-polish (msl-sub (msl-num 45) (msl-num 30))) (s-exp->list '(45 30 -)))
(test (output-reverse-polish (msl-mult (msl-num 1) (msl-num 478))) (s-exp->list '(1 478 *)))
(test (output-reverse-polish (msl-mult (msl-num 6) (msl-num 7))) (s-exp->list '(6 7 *)))
(test (output-reverse-polish (msl-add (msl-num 3) (msl-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish (msl-add (msl-mult (msl-num 5) (msl-num 6)) (msl-num 7))) (s-exp->list '(5 6 * 7 +)))
(test (output-reverse-polish (msl-sub (msl-num 7) (msl-add (msl-num 3) (msl-num 5)))) (s-exp->list '(7 3 5 + -)))
(test (output-reverse-polish (msl-add (msl-num 4) (msl-add (msl-num 8) (msl-num 10)))) (s-exp->list '(4 8 10 + +)))

;; examples
;; (msl-num 5) -> '(5)
;; (msl-add (msl-num 5) (msl-num 9)) -> '(5 9 +)
;; (msl-mult (msl-num 10) (msl-num 5)) -> '(10 5 *)
;; (msl-add (msl-mult (msl-num 7) (msl-num 8)) (msl-num 9)) -> '(7 8 9 + *)
;; (parse-prefix '(* 10 (* 2 3))) -> '(10 2 3 * *)
;; (eval(parse-prefix '(* 10 (* 2 3)))) -> 60
"Examples"
(output-reverse-polish (msl-num 5))
(output-reverse-polish (msl-add (msl-num 5) (msl-num 9)))
(output-reverse-polish (msl-mult (msl-num 10) (msl-num 5)))
(output-reverse-polish (msl-mult (msl-num 7) (msl-add (msl-num 8) (msl-num 9))))
(output-reverse-polish (parse-prefix '(* 10 (* 2 3))))
(eval (parse-prefix '(* 10 (* 2 3))))

;unparser-infix
(define (unparser-infix [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
      (msl-add (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '+))) (unparser-infix rhs)))
      (msl-mult (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '*))) (unparser-infix rhs)))
      (msl-sub (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '-))) (unparser-infix rhs)))
      (msl-div (lhs rhs) (append (append (unparser-infix lhs) (list (symbol->s-exp '/))) (unparser-infix rhs)))
    ))

;test

(test (unparser-infix (msl-num 1)) (s-exp->list '(1)))
(test (unparser-infix (msl-num 2)) (s-exp->list '(2)))
(test (unparser-infix (msl-num 3)) (s-exp->list '(3)))
(test (unparser-infix (msl-add (msl-num 5) (msl-num 7))) (s-exp->list '( 5 + 7)))
(test (unparser-infix (msl-add (msl-num 4) (msl-num 9))) (s-exp->list '( 4 + 9)))

;unparser-prefix
(define (unparser-prefix [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
    (msl-add (lhs rhs) (append (list(symbol->s-exp '+)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
    (msl-mult (lhs rhs) (append (list(symbol->s-exp '*)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
    (msl-sub (lhs rhs) (append (list(symbol->s-exp '-)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
    (msl-div (lhs rhs) (append (list(symbol->s-exp '/)) (append (unparser-prefix lhs) (unparser-prefix rhs))))
    ))

;test

(test (unparser-prefix (msl-num 1)) (s-exp->list '(1)))
(test (unparser-prefix (msl-num 2)) (s-exp->list '(2)))
(test (unparser-prefix (msl-add (msl-num 7) (msl-num 8))) (s-exp->list '(+ 7 8)))
(test (unparser-prefix (msl-mult (msl-num 7) (msl-num 8))) (s-exp->list '(* 7 8)))

;;-----------------------------------------------------------------------------------------------------------------------------------------------------

; SUGAR LANGUAGE

(define-type msls
  [num-msls (n : number)]
  [plus-msls (l : msls) (r : msls)]
  [mult-msls (l : msls) (r : msls)]
  [bmin-msls (l : msls)] 
  [umin-msls (r : msls)]
  [sub-msls (l : msls) (r : msls)] 
  )

;;Desugar method
;; msls -> msl
;; Purpose : Make the number negative
;; Template :
; (define (desugar [sugar : msls]) : msl
;  (type-case msls sugar
;   [num-msls (n) ..]
; [plus-msls (l r) ..]
; [mult-msls (l r) ..]
; [sub-msls (l r) ..]
; [bmin-msls (v) ..]
; [umin-msls (v) ..]
  
  (define (desugar [sugar : msls]) : msl
  (type-case msls sugar
    [num-msls (n) (msl-num n)]
    [plus-msls (l r) (msl-add
                      (desugar l)
                      (desugar r))]
    [mult-msls (l r) (msl-add
                      (desugar l)
                      (desugar r))]
    [sub-msls (l r) (msl-sub     
                     (desugar l) 
                     (desugar r))]
    [bmin-msls (v) (msl-mult
                       (desugar v)
                       (msl-num -1
                       ))]
    [umin-msls (v) (desugar (sub-msls (num-msls 0) v))] 
    ))

;;testing desugar
;; (msls-num 9) -> 9
;; (plus-msls (num-msls 8) umin-msls (num-msls 5))
;; (msl-add (msl-num 8) (msl-add (msl-num 0) (msl-mult (msl-num -1) (msl-num 5)
;; (div-msls (num-msls 4) (num-msls 2)
;; (msl-div (msl-num 4) (msl-num 2)

    
(test (desugar (num-msls 9)) (msl-num 9))
(test (desugar (plus-msls (num-msls 8) (umin-msls (num-msls 5))))
      (msl-add (msl-num 8) (msl-sub (msl-num 0) (msl-num 5))))
(test (desugar (plus-msls (num-msls 8) (bmin-msls (num-msls 5))))
      (msl-add (msl-num 8) (msl-mult (msl-num 5) (msl-num -1))))
(test (desugar (sub-msls (num-msls 4) (num-msls 2)))
      (msl-sub (msl-num 4) (msl-num 2)))

"------Assignment3-----"

;; expt : number number -> number
;; Purpose: To calculate exponentiation of given two number, first number base and second is power.
(define (pow (a : number) (b : number)) : number
  (cond
    ((= b 0) 1)
    ((even? b) (sqr (pow a (/ b 2))))
    (else (* a (pow a (- b 1))))))

;; sqr : number -> number
;; Purpose: To calculate square of given number.
(define (sqr (a : number)) : number
  (* a a))

;; Pair is a well-known data structure in Lisp/Scheme family languages,
;; - since we do not have a data structure in plai-type, 
;; - this is an basic implementation of it.
(define-type pair
  (sym-op (sym : symbol)(op : (number number -> number))))

;; A table for operations, 
;; - by changing just this data structure,
;; - you can add any binary operations.

;; A list of pair(sym-op) as table of operations.
;; Handycap of this is, 
;; - it is complety depending on host language's operations.
(define ops
  (list
   (sym-op '+ +)
   (sym-op '* *)
   ;; Several binary operations added as it seen below
   (sym-op '- -)
   (sym-op '/ /)
   (sym-op '^ pow)
   (sym-op 'custom (lambda (x y) (+ (* 2 x) y)))
   ))

;; get-op : symbol -> ((number number) -> number)
;; Purpose : To obtain binary defined operation from operation definition table.
(define (get-op (sym : symbol)) : (number number -> number)
  (sym-op-op (assoc sym ops)))

;; assoc : symbol (listof pair) -> pair
;; Purpose : To associate given symbol with operation defined in a listof pairs.
(define (assoc (s : symbol) (lp : (listof pair))) : pair
  (let ((list-op (filter (lambda (x) (eq? s (sym-op-sym x))) lp)))
    (if (empty? list-op)
        (error 'assoc "Operation not defined")
        (first list-op))))

; Grammar for ExprC.
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [binaryOpC (op : symbol) (l : ExprC) (r : ExprC)]
  [ifZeroC (pred : ExprC)(trueState : ExprC)(falseState : ExprC)])

;; parse : s-exp -> ExprC
;; Purpose : To parse given s-exp to abstract syntax ExprC
;; Template : 
;(define (parse [s : s-expression]) : ExprC
;  (cond
;    [n ...]
;    [id ...]
;    any unary or binary function
;    ))

(define (parser [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'ifzero (s-exp->symbol (first sl)))
              (ifZeroC (parser (second sl))
                       (parser (third sl))
                       (parser (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (binaryOpC (s-exp->symbol (first sl)) 
                     (parser (second sl)) (parser (third sl)))]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parser (second sl)))]
         [else (error 'parser "invalid list input")])
       )]
    [else (error 'parser "invalid input")]))

;; Tests :
(test (parser (number->s-exp 5))(numC 5))
(test (parser (symbol->s-exp 'x))(idC 'x))
(test (parser '(+ 3 4))(binaryOpC '+ (numC 3)(numC 4)))
(test (parser '(* 3 4))(binaryOpC '* (numC 3)(numC 4)))
(test (parser '(+ x x))(binaryOpC '+ (idC 'x)(idC 'x)))
(test (parser '(* x x))(binaryOpC '* (idC 'x)(idC 'x)))
(test (parser '(f (* x x)))(appC 'f (binaryOpC '* (idC 'x)(idC 'x))))

(test (parser '(ifzero 4 5 6))(ifZeroC (numC 4)(numC 5)(numC 6)))
(test (parser '(ifzero (- 3 4) 5 6))(ifZeroC 
                                    (binaryOpC '- (numC 3)(numC 4))
                                    (numC 5)(numC 6)))
(test
 (parser 
  '(ifzero (factorial n) 1
           (* n (factorial (sub1 n)))))
 (ifZeroC
  (appC 'factorial (idC 'n))
  (numC 1)
  (binaryOpC
   '*
   (idC 'n)
   (appC 'factorial (appC 'sub1 (idC 'n))))))

;; Tests :
parser (number->s-exp 5)(numC 5)
(parser (symbol->s-exp 'x))(idC 'x)
(parser '(+ 3 4))(binaryOpC '+ (numC 3)(numC 4))
(parser '(* 3 4))(binaryOpC '* (numC 3)(numC 4))
(parser '(+ x x))(binaryOpC '+ (idC 'x)(idC 'x))
(parser '(* x x))(binaryOpC '* (idC 'x)(idC 'x))
(parser '(f (* x x)))(appC 'f (binaryOpC '* (idC 'x)(idC 'x)))

(parser '(ifzero 4 5 6))(ifZeroC (numC 4)(numC 5)(numC 6))
(parser '(ifzero (- 3 4) 5 6))(ifZeroC 
                                    (binaryOpC '- (numC 3)(numC 4))
                                    (numC 5)(numC 6))
(parser 
  '(ifzero (factorial n) 1
           (* n (factorial (sub1 n)))))
 (ifZeroC
  (appC 'factorial (idC 'n))
  (numC 1)
  (binaryOpC
   '*
   (idC 'n)
   (appC 'factorial (appC 'sub1 (idC 'n)))))


;; Function Definition Structure
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; Example function definition namespace.
(define FuncDefNameSpace
  (list
   (fdC 'sqr 'x (parser '(* x x)))
   (fdC 'sub1 'x (parser '(+ x -1)))
   (fdC 'neg 'x (parser '(* x -1)))
   (fdC 'double 'x (parser '(+ x x)))
   (fdC 'quadruple 'x (parser '(double (double x))))
   (fdC 'const5 '_ (parser (number->s-exp 5)))
   (fdC 'factorial 'n (parser 
                       '(ifzero n 1
                                (* n (factorial (sub1 n))))))
  ))



;; get-fundef : symbol (listof FunDefC) -> FunDefC
;; Purpose : To find given symbol's(function name/identifier) function definition
;; - from function definition namespace.
;; Template : Basic Structural Recursion
; (define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
;  (cond
;    [(empty? fds) ...]
;    [else ...(first fds) ...(get-fundef (rest fds))])

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))
;; Tests:
(test (get-fundef 'sub1 FuncDefNameSpace) 
      (fdC 'sub1 'x (parser '(+ x -1))))
(test (get-fundef 'neg FuncDefNameSpace) 
      (fdC 'neg 'x (parser '(* x -1))))
(test (get-fundef 'sqr FuncDefNameSpace) 
      (fdC 'sqr 'x (parser '(* x x))))

"Example->"
(get-fundef 'sub1 FuncDefNameSpace) 
      (fdC 'sub1 'x (parser '(+ x -1)))
(get-fundef 'neg FuncDefNameSpace) 
      (fdC 'neg 'x (parser '(* x -1)))
(get-fundef 'sqr FuncDefNameSpace) 
      (fdC 'sqr 'x (parser '(* x x)))

;; Binding is a data type to bind value with identifiers.
(define-type Binding
  [bind (name : symbol) (val : number)])

;; Just an alias to keep it clean, wrapper around listof Bindings.
(define-type-alias Environment (listof Binding))

;; Empty environment.
(define mt-env empty)

;; Extending environment a wrapper around cons.
(define extend-env cons)

;; Example Environment.
(define EnvNameSpace
  (list
   (bind 'x 5)
   (bind 'y 6)
   (bind 'z 7)
   ))

;; lookup : symbol (listof Bindings) -> number
;; Purpose : To find given symbol's value
;; - from environment(listof bindings).
;; Template : Basic Structural Recursion
; (define (lookup [for : symbol] [env : Environment]) : number
;  (cond
;    [(empty? env) ...]
;    [else ...(first env) ...(lookup (rest env))])

(define (lookup [for : symbol] [env : Environment]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

;; Tests:
(test (lookup 'x EnvNameSpace) 5)
(test (lookup 'y EnvNameSpace) 6)
(test (lookup 'z EnvNameSpace) 7)

"Example->"


(lookup 'x EnvNameSpace) 5
(lookup 'y EnvNameSpace) 6
(lookup 'z EnvNameSpace) 7
; ERROR CASE : (test (lookup 'w EnvNameSpace) 'error)

;; interp : ExprC (listof FunDefC) -> number
;; Purpose : To evaluate expressions to numbers.
;; Template :
; (define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
;  (type-case ExprC in
;    [numC (n) ...]
;    [idC (s) ...]
;    [appC (f a) ...]
;    [binaryOpC (l r) ...]

(define (interp [e : ExprC] [env : Environment][fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (s) (lookup s env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env 
                           (bind (fdC-arg fd)(interp a env fds)) ;Make it eager!
                          
                           mt-env);İf it is lazy!
                          fds))]
    [binaryOpC (op l r)((get-op op)
                        (interp l env fds)
                        (interp r env fds))]
    ;; ifZero : ExprC Expr ExprC -> ExprC
    ;; an f statement that controls if first argument is zero 
    ;; -- or not, if it is zero that returns second argunment,
    ;; --- otherwise third argument. Partially lazy.
    [ifZeroC (pred t f)
             (if (= 0 (interp pred env fds))
                 (interp t env fds)
                 (interp f env fds))]
    ))

;; Tests:
;; From book as is stated in worksheet !
(test (interp (parser '(+ 10 (const5 10)))
              mt-env
              FuncDefNameSpace) 15)
(test (interp (parser '(+ 10 (double (+ 1 2))))
              mt-env
              FuncDefNameSpace) 16)
(test (interp (parser '(+ 10 (quadruple (+ 1 2))))
              mt-env
              FuncDefNameSpace) 22)

"Example->"

(interp (parser '(+ 10 (const5 10)))
              mt-env
              FuncDefNameSpace) 15
(interp (parser '(+ 10 (double (+ 1 2))))
              mt-env
              FuncDefNameSpace) 16
(interp (parser '(+ 10 (quadruple (+ 1 2))))
              mt-env
              FuncDefNameSpace) 22
; ERROR CASE : (interp (parse '(f1 3))
;        mt-env
;        (list (fdC 'f1 'x (parse '(f2 4)))
;              (fdC 'f2 'y (parse '(+ x y)))))


;; eval : s-exp -> number
;; Purpose : A wrapper function to evaluate s-exp through our language.
(define (evals (sexp : s-expression)) : number
  ;(interp (parse sexp) empty))
  (interp (parser sexp) mt-env FuncDefNameSpace))

;; Tests:
;;; SAME TEST CASES THAT WE DID WITH SUBSTITUTION MODEL SO TO OBSERVE,
;;; -- SAME BEHAVIOUR !!!

(test (interp (parser (number->s-exp 3)) mt-env empty) 3)
;ERROR CASE : (test (interp (parse (symbol->s-exp 'x)) empty) 
;  (error 'interp ""shouldn't get here""))
(test (evals '(+ 3 4)) 7)
(test (evals '(* 3 4)) 12)
(test (evals '(sqr 4)) 16)
(test (evals '(neg 4)) -4)
(test (evals '(/ 3 4)) (/ 3 4)) ;; Racket numbers and operations rocks !!
(test (evals '(^ 3 4)) 81)
(test (evals '(- 3 4)) -1)
(test (evals'(custom 3 4)) 10)
(test (evals '(factorial 0)) 1)
(test (evals '(factorial 1)) 1)
(test (evals '(factorial 5)) 120)
(test (evals '(factorial 7)) 5040)

"Example ->"

(evals '(+ 3 4)) 7
(evals '(* 3 4)) 12
(evals '(sqr 4)) 16
(evals '(neg 4)) -4
(evals '(/ 3 4)) (/ 3 4) ;; Racket numbers and operations rocks !!
(evals '(^ 3 4)) 81
(evals '(- 3 4)) -1
(evals '(custom 3 4)) 10
(evals '(factorial 0)) 1
(evals '(factorial 1)) 1
(evals '(factorial 5)) 120
(evals '(factorial 7)) 5040

"CLASSWORK6"  ;Free identifier koymadım çünkü proje 7 geliştirilmiş hali.

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





