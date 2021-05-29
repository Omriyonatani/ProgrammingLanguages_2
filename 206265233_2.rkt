#lang pl 02

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 - BNF (SE) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Section a - SE BNF

#|
Valid programs where:

--Numbers rules--
# <D> stands for Digit-
  <D> ::= 0|1|2|3|4|5|6|7|8|9

--Strings rules--
# λ represents empty string.


<NumE> ::=  <Num>                                           ;[1]
          | <D>                                             ;[2]
          | <NumE <D>>                                      ;[3]
          | (string-length <StringE>)                       ;[4]


<CharE> ::=  <Char>                                         ;[5]
           | <CharE <Char>>                                 ;[6]


<StringE> ::=  <String>                                     ;[7]
             | <λ <String>>                                 ;[8]
             | (string <CharE>)                             ;[9]
             | (string-append <String> <StringE>)           ;[10]
             | (string-insert <StringE> <Char> <Number>)    ;[11]
             | (number->string <NumE>)                      ;[12]


# Where : NumE    = Number
          CharE   = Char
          StringE = String


<SE> ::=  <SE>
        | <NumE>
        | <CharE>
        | <StringE>

|#


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Section b - Derivation process

#|


1.(string-insert (string #\1 #\2 #\3) #\4 3) - Derivation replacements:
(string-insert ("123" [rule 9]) #\4 3)
(string-insert ("123" #\4 [rule 6->5: CharE] 3)
(string-insert ("123" #\4 3 [rule 2->1: NumE])
(string-insert("123444"[rule 11])
("123444") [rule 7]
"123444" <StringE>
<SE>

2.(number->string (string-length "00000000000" [rule4])) - Derivation replacements:
(number->string (11[rule 3])
(number->string (11[rule 3->1: NumE])
(number->string (11[rule 12])
("11") [rule 7]
"11" <StringE>
<SE>


3.(string-append (string #\1 #\1 #\1) λ) - Derivation replacements:
(string-append ("111"[rule 9->7] λ)
(string-append ("111" λ[rule 8->7])
(string-append ("111" λ[rule 10])
"111 "[rule 7]
"111 " <StringE>
<SE>


|#




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 - Higher Order Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(: square : Number -> Number)
;; Square function- get as input a number, and returns the square number.
;; input- Number x
;; output- Number x*x
;; * Number include negative\positive numbers.
;; The function takes the number and make a multiply with this number again.
;; I have not encountered any difficulties in writing this function
;; I will use this function as a help function for "sum-of-squares" function.
(define (square x)
  (* x x))


(: sum-of-squares : (Listof Number) -> Number)
;; Sum-of-squares function calculate the sum of the squares numbers of given list.
;; The function get as input a list of numbers,
;; calculate their squares by "square" function,
;; and sum all of them to the result number, that initiallize to be zero at the beginning.
;; The main difficult to solve that question is to understand the "map" and "foldl" function from the documentation.
;; I solved it after a 2 hours of read doc and tries.. (include the square function)
(define (sum-of-squares list)
  (foldl + 0 (map square list)))

;; ~~~ Tests ~~~ square
(test (square 1) => 1)
(test (square -1) => 1)
(test (square 10) => 100)
(test (square 2) => 4)
(test (square -2) => 4)
(test (square 0) => 0)
(test (square -0) => 0)



;; ~~~ Tests ~~~ sum-of-squares
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 -2 3)) => 14)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(0 2 3)) => 13)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(-0 -0 -0)) => 0)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(3 3 3)) => 27)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1 1 1 1 1 1 1 1 1 1)) => 10)
(test (sum-of-squares '(2 2 2 2 2 2 2 2 2)) => 36)
(test (sum-of-squares '(2 -2 2 -2 2 -2 2 -2 2)) => 36)
(test (sum-of-squares '(-2 -2 -2 -2 -2 -2 -2 -2 -2)) => 36)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 - PAE (and more Higher Order Functions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Section a - createPolynomial


(: createPolynomial : (Listof Number) -> (Number -> Number))
;; The "createPolynomial" function, takes list of numbers (coefficients) and returns function that get number and return number.
;; I used the function to return polyX, this is a function that get number as input, and returns number as output.
;; My function will work with the correct input.
;; The function get the input and call to "poly" and "polyX". return "polyX" function. 
;; The main difficult to solve that question is to know how to work with function that returns function,
;; and to understand how it have to look like. (i used the examples a lot).
;; I solved it after a 2.5 hours of tries..
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  ;; The "poly" function, takes a list of numbers (coefficients) from "createPolynomial" and making the calculate for each 'X' in the polynom.
  ;; The calculate includes:
  ;; argsL= coefficients
  ;; x= the x value from the input
  ;; power= the function calculate the power for each 'X', by "add1" everytime.. (x^0, x^1, x^2.. etc..)
  ;; accum= the accumulate of the polynom.
  ;; I used tail-recursive, that accumulate the answer and return it by the end.
  ;; The recursive call checks if there is a null list?, it true- return the accumulate until there,
  ;; if false- keep calculate the polynom.
  ;; My function will work with call from polyX only (with the initiallization values).
  ;; The main difficult to solve that question is to understand how to calculate the polynom rules.
  ;; I solved it after a 1.5 hour (included at the 2.5 hours for the main function).
  (define (poly argsL x power accum)
    (if (null? argsL) accum
        (poly (rest argsL) x (+ 1 power) (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number)
  ;; The "polyX" function, takes number as input, this is the "X" value, and call to "poly" function with the initiallization values.
  ;; I used "0" value where is initiallization because "zero" is neutral for Plus+.
  ;; My function will work with valid "X" value only.
  ;; The main difficult to solve that question is to understand the struct of the function and the way is run.
  ;; I solved it after half a hour.
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)


;; ~~~ Tests ~~~ createPolynomial
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)

(define p-2-3-4-5 (createPolynomial '(-2 -3 -4 -5)))
(test (p-2-3-4-5 0) => (+ (* -2 (expt 0 0)) (* -3 (expt 0 1)) (* -4 (expt 0 2)) (* -5 (expt 0 3))))
(test (p-2-3-4-5 4) => (+ (* -2 (expt 4 0)) (* -3 (expt 4 1)) (* -4 (expt 4 2)) (* -5 (expt 4 3))))
(test (p-2-3-4-5 11) => (+ (* -2 (expt 11 0)) (* -3 (expt 11 1)) (* -4 (expt 11 2)) (* -5 (expt 11 3))))

(define p-530 (createPolynomial '(-5 3 0)))
(test (p-530 11) => (+ (* -5 (expt 11 0)) (* 3 (expt 11 1)) (* 0 (expt 11 2))))

(define p_0000 (createPolynomial '(0 0 0 0)))
(test (p_0000 0) => 0)

(define p111 (createPolynomial '(1 1 1)))
(test (p111 1) => (+ (* 1 (expt 1 0)) (* 1 (expt 1 1)) (* 1 (expt 1 2))))





;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Section b-
;; section i-

#|


  The grammar:
  <PLANG> ::=  {Poly { AEs } { AEs }}

  <AEs>   ::=  <AE>
             | <AE> <AEs>

  <AE>    ::=  <Num>
             | { + <AE> <AE> }
             | { - <AE> <AE> }
             | { * <AE> <AE> }
             | { / <AE> <AE> }

|#




;; section ii-

;; PLANG type, is a Poly with 2 lists of AE
(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

;; AE type
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressiong into AEs
;; input: S-expression
;; output: AE
;; purpose: To convert sexpr to an AE type. 
;; operates: That function convert sexpr type to AE type, to "help" the other function- parse.
;; The function checks by "match" function the input, and works by the logic of the AE.
;; If there is not a good syntax- return Error.
;; My function will work with Sexpr only.
;; I didnt have any difficulty.
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



;; ~~~ Tests ~~~ parse-sexpr

(test (parse-sexpr '((poly 1 2 3) (1 -1 -0)))
      =error> "parse-sexpr: bad syntax in ((poly 1 2 3) (1 -1 0))")

(test (parse-sexpr '((poly 1 1 1) (1 2 -0)))
      =error> "parse-sexpr: bad syntax in ((poly 1 1 1) (1 2 0))")

(test (parse-sexpr '((poly -1 -2 -3) (-1 -1 -0)))
      =error> "parse-sexpr: bad syntax in ((poly -1 -2 -3) (-1 -1 0))")

(test (parse-sexpr '((poly 0 0 0) (0 0 0)))
      =error> "parse-sexpr: bad syntax in ((poly 0 0 0) (0 0 0))")

(test (parse-sexpr (+ 1 2)) => (Num 3))
(test (parse-sexpr (+ 0 0)) => (Num 0))
(test (parse-sexpr (* 1 2)) => (Num 2))
(test (parse-sexpr (* 1 0)) => (Num 0))
(test (parse-sexpr (/ 1 2)) => (Num 1/2))
(test (parse-sexpr (/ 2 13)) => (Num 2/13))
(test (parse-sexpr (- 1 2)) => (Num -1))
(test (parse-sexpr (- 1 0)) => (Num 1))
(test (parse-sexpr (+ 11 2)) => (Num 13))
(test (parse-sexpr (+ 0 1)) => (Num 1))
(test (parse-sexpr (* 1 -2)) => (Num -2))
(test (parse-sexpr (* 1 -0)) => (Num 0))
(test (parse-sexpr (/ -1 2)) => (Num -1/2))
(test (parse-sexpr (/ 0 13)) => (Num 0))
(test (parse-sexpr (- 1 10)) => (Num -9))
(test (parse-sexpr (- 0 0)) => (Num 0))
(test (parse-sexpr (+ (+ 1 0) 2)) => (Num 3))
(test (parse-sexpr (+ (+ 0 0) (+ 0 0) 0)) => (Num 0))
(test (parse-sexpr (* (* 1 1) 1 2)) => (Num 2))
(test (parse-sexpr (* (* 1 0) 0)) => (Num 0))
(test (parse-sexpr (/ (/ 1 1) 2)) => (Num 1/2))
(test (parse-sexpr (/ ( + (* 1 1) 2) 13)) => (Num 3/13))
(test (parse-sexpr (- (* (+ 1 1) 2) 5)) => (Num -1))
(test (parse-sexpr (- 1 0)) => (Num 1))
(test (parse-sexpr (/ ( + (* (- 0 0) 1 1) 2) 13)) => (Num 2/13))
(test (parse-sexpr (/ ( + (* (- (* 1 1) 0) 1 1) 2) 13)) => (Num 3/13))



(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
;; input: String that represents PLANG
;; output: PLANG
;; purpose: To convert String to PLANG type. 
;; operates: That function convert String type that represents a PLANG to PLANG type that makes "Polynom".
;; My function will work with correct String only.
;; The function get the String, make it to be a sexpr by "string->sexpr" function, and check the sexpr:
;; by match function the check the pattern and by cond function to check the validation.
;; if the pattern not good- Error,
;; if the validation not good- another Error.
;; The main difficult to solve that question is to know how to "check" the String for validation. (syntax problems..) 
;; I solved it after a 3 hour. its was not easy for me.
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(list (list 'poly a ...) (list b ...))
       (cond
         [(and (> (length a) 0) (> (length b) 0)) (Poly (map parse-sexpr a) (map parse-sexpr b))]
         [(< (length a) 1) (error 'parse "at least one coefficient is required in ~s" code)]
         [else (error 'parse "at least one point is required in ~s" code)])]
      [else (error 'parse "bad syntax in ~s" code)])))


;; ~~~ Tests ~~~ parse

(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))

(test (parse "{{poly 1 1 1} {1 1 1}}")
      => (Poly (list (Num 1) (Num 1) (Num 1))
               (list (Num 1) (Num 1) (Num 1))))

(test (parse "{{poly -1 2 -3} {-1 2 -3}}")
      => (Poly (list (Num -1) (Num 2) (Num -3))
               (list (Num -1) (Num 2) (Num -3))))

(test (parse "{{poly 1 2 3} {-1 -2 -3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num -1) (Num -2) (Num -3))))

(test (parse "{{poly 0 0 0} {0 0 0}}")
      => (Poly (list (Num 0) (Num 0) (Num 0))
               (list (Num 0) (Num 0) (Num 0))))

(test (parse "{{poly -1 -2 -3} {-1 -2 -3}}")
      => (Poly (list (Num -1) (Num -2) (Num -3))
               (list (Num -1) (Num -2) (Num -3))))

(test (parse "{{poly {+ 1 1} 7 {/ 5 10} 11 {- 5 3} {* 2 1}} {{+ 2 1} 2 {/ 1 1} 3 {- 10 2} {* 2 4}}}")
      => (Poly (list (Add (Num 1) (Num 1)) (Num 7)  (Div (Num 5) (Num 10)) (Num 11)  (Sub (Num 5) (Num 3)) (Mul (Num 2) (Num 1)))
               (list (Add (Num 2) (Num 1)) (Num 2)  (Div (Num 1) (Num 1)) (Num 3)  (Sub (Num 10) (Num 2)) (Mul (Num 2) (Num 4)))))
(test (parse "{{poly {+ 0 0} 0 {/ 0 10} 0 {- 5 5} {* 2 0}} {{+ 0 0} 0 {/ 0 1} 0 {- 10 10} {* 2 0}}}")
      => (Poly (list (Add (Num 0) (Num 0)) (Num 0)  (Div (Num 0) (Num 10)) (Num 0)  (Sub (Num 5) (Num 5)) (Mul (Num 2) (Num 0)))
               (list (Add (Num 0) (Num 0)) (Num 0)  (Div (Num 0) (Num 1)) (Num 0)  (Sub (Num 10) (Num 10)) (Mul (Num 2) (Num 0)))))
(test (parse "{{poly {+ 10 10} 7 {/ 4 10} 11 {- 5 3} {* 2 1}} {{+ 2 1} 2 {/ 1 7} 3 {- 10 2} {* 2 9}}}")
      => (Poly (list (Add (Num 10) (Num 10)) (Num 7)  (Div (Num 4) (Num 10)) (Num 11)  (Sub (Num 5) (Num 3)) (Mul (Num 2) (Num 1)))
               (list (Add (Num 2) (Num 1)) (Num 2)  (Div (Num 1) (Num 7)) (Num 3)  (Sub (Num 10) (Num 2)) (Mul (Num 2) (Num 9)))))

(test (parse "{{poly } {}}")
      =error> "parse: at least one coefficient is required in ((poly) ())")

(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))")

(test (parse "{{poly } {1 1 1 1} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 1 1 1))")

(test (parse "{{poly 1 2 2 1} {} }")
      =error> "parse: at least one point is required in ((poly 1 2 2 1) ())")

(test (parse "{{poly 1} {} }")
      =error> "parse: at least one point is required in ((poly 1) ())")

(test (parse "{11 5}")
      =error> "parse: bad syntax in (11 5)")
(test (parse "{0 poly 87}")
      =error> "parse: bad syntax in (0 poly 87)")

(test (parse "{poly poly poly 1}")
      =error> "parse: bad syntax in (poly poly poly 1)")





;; section iii-

;; evaluates AE expressions to numbers
(: eval : AE -> Number)
;; input: AE
;; output: Number
;; purpose: To convert an AE type to Number. 
;; operates: That function convert AE type to a Number type, to evaluate the Polynom.
;; My function will work with AE only.
;; There no any difficulty.
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))



;; ~~~ Tests ~~~ eval

(test (eval (Num -1)) => -1)
(test (eval (Num 1)) => 1)
(test (eval (Num -0)) => 0)
(test (eval (Num 0)) => 0)
(test (eval (Add (Num 2)(Num -1))) => 1)
(test (eval (Add (Num 2)(Num 11))) => 13)
(test (eval (Sub (Num 2)(Num -1))) => 3)
(test (eval (Sub (Num 2)(Num 1))) => 1)
(test (eval (Mul (Num 2)(Num -1))) => -2)
(test (eval (Mul (Num 2)(Num 11))) => 22)
(test (eval (Mul (Num 0)(Num 10))) => 0)
(test (eval (Mul (Num 0)(Num 0))) => 0)
(test (eval (Div (Num 0)(Num -1))) => 0)
(test (eval (Div (Num 2)(Num 1))) => 2)
(test (eval (Div (Num 100)(Num -10))) => -10)
(test (eval (Div (Num 10)(Num 10))) => 1)
(test (eval (Div (Div (Num 100) (Num 10)) (Num -10))) => -1)
(test (eval (Mul (Div (Num 10) (Num 10)) (Num -10))) => -10)
(test (eval (Add (Div (Num 100) (Num 10)) (Num -10))) => 0)
(test (eval (Add (Div (Sub (Num 100)(Num 100)) (Num 10)) (Num 9))) => 9)




(: eval-poly : PLANG -> (Listof Number))
;; input: PLANG
;; output: List of Numbers
;; purpose: To evaluate PLANG to an List of Number type. 
;; operates: That function convert and evaluate a PLANG type to List of Number type, and uses the "eval" function from above.
;; The function takes the Poly, with coefficients and X'es, that comes in 2 lists of AE, and makes the calculate.
;; The function "map" helps me to go all over the items in the lists.
;; And i used the "createPolynomial" function, that i maked in section 2.
;; My function will work with PLANG only.
;; The main difficult to solve that question is to understand the mathematics behind the evaluate, to make a scheme on the paper and to know to use "map" and "createPolynomial" functions very well. 
;; I solved it after a 2.5 hour of tries and calculates..
(define (eval-poly p-expr)
  (cases p-expr
    [(Poly coeff exes) (map (createPolynomial (map eval coeff)) (map eval exes))]))


;; ~~~ Tests ~~~ eval-poly

(test (eval-poly (Poly (list (Num 0) (Num 0) (Num 0))
                       (list (Num 0) (Num 0) (Num 0))))
      => '(0 0 0))

(test (eval-poly (Poly (list (Num 1) (Num 1) (Num 1))
                       (list (Num 1) (Num 1) (Num 1))))
      => '(3 3 3))

(test (eval-poly (Poly (list (Num 5) (Num 0) (Num 1))
                       (list (Num -1) (Num -2) (Num -3))))
      => '(6 9 14))

(test (eval-poly (Poly (list (Num -1) (Num -2) (Num -3))
                       (list (Num 1) (Num 5) (Num 10))))
      => '(-6 -86 -321))

(test (eval-poly (Poly (list (Num 1) (Num 1) (Num 0))
                       (list (Num 0) (Num 2) (Num 5))))
      => '(1 3 6))



(: run : String -> (Listof Number))
;; evaluate a PLANG program contained in a string
;; input: String
;; output: List of Numbers
;; purpose: To run the program. 
;; operates: That function uses "eval-poly" and "parse" functions and making the program to run.
;; My function will work with valid String only.
;; There is no some difficult to solve that question. 
(define (run str)
  (eval-poly (parse str)))




;; ~~~ Tests ~~~ run

(test (run "{{poly 1 2 3} {1 2 3}}")
      => '(6 17 34))

(test (run "{{poly 4 2 7} {1 4 9}}")
      => '(13 124 589))

(test (run "{{poly 1 2 3} {1 2 3}}")
      => '(6 17 34))

(test (run "{{poly 4/5 } {1/2 2/3 3}}")
      => '(4/5 4/5 4/5))

(test (run "{{poly 2 3} {4}}")
      => '(14))

(test (run "{{poly 1 1 0} {-1 3 3}}")
      => '(0 4 4))

(test (run "{{poly 0 0 0} {-1 3 3}}")
      => '(0 0 0))

(test (run "{{poly 1 1 1} {-1 7 3}}")
      => '(1 57 13))

(test (run "{{poly -1 1 0} {-1}}")
      => '(-2))

(test (run "{{poly -1 -1 -0} {-9}}")
      => '(8))

(test (run "{{poly -1 -1 -0} {0}}")
      => '(-1))

(test (run "{{poly -1 -1 -1 1 1 1 1} {0}}")
      => '(-1))

(test (run "{{poly -1 -1 -1 1 1 1 1} {0 1 2}}")
      => '(-1 1 113))
