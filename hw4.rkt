#lang racket

(provide 
         entry entry-key entry-value entry?
         bnot bnot-arg bnot?
         band band-arg1 band-arg2 band?
         bor bor-arg1 bor-arg2 bor?
         tt tt-vars tt-rows tt?
	 hours
         lookup unique-keys?
         boolean-exp? type-of
	 all-vars
	 eval-in-env
	 all-combs
	 truth-table	 	 	 
	 satisfiable? equivalent?
	 find-exp
         substitute-in
         match)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201 HW #4  DUE 11:59 pm Monday, October 23, 2023
; using the submit command on the Zoo.
; ****************************************************************
; Name: Naalah Cohen
; Email address: naalah.cohen@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 12)

; ****************************************************************
; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The topics of this assignment are as follows.
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, satisfiability, equivalence.

; You might also find uses for the special form: case.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define test1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define test2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (9 points)
; Write two procedures to deal with tables as follows.

; (lookup key table)
; (unique-keys? table)

; (lookup key table)
; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; (unique-keys? table)
; returns #t if all the entries of table have keys that are pairwise
; not equal? and #f otherwise.

; Examples
;> (lookup "first" test1)
;1
;> (lookup "third" test1)
;#f
;> (lookup 'z test2)
;1
;> (unique-keys? test1)
;#t
;> (unique-keys? test2)
;#f
;>
; ****************************************************************

(define (lookup key table)
  (cond
    [(empty? table) #f]
    [(equal? (entry-key (car table)) key) (entry-value (car table))]
    [else
     (lookup key (cdr table))]
    )
  )


;looks at everything after the key value entry to see if it is in the rest of the list
; ex: lst = list (entry "first" 5) (entry "second" 5) --> (unique lst) #f
; ex: lst = list (entry "first" 5) (entry "second" 6) --> (unique lst) #t
(define (uniquehelp table)
  (if (equal? (lookup (entry-key (car table)) (cdr table)) #f) #t
      #f
  )
 )

(define (unique-keys? table)
  (cond
    [(empty? table) #t]
    [(equal? (uniquehelp table) #t) (unique-keys? (cdr table))]
    [else
     #f
     ]
    )
  )
  

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions.  Note that the transparent attribute allows
; the contents of a structure to be printed out, and allows two
; structures to be compared for content using equal?

(struct bnot (arg) #:transparent)
(struct band (arg1 arg2) #:transparent)
(struct bor (arg1 arg2) #:transparent)

; We recursively define a representation of Boolean expressions as follows.

; 1) 0 and 1 represent the constants 0 and 1 and are valid Boolean
;    expressions

; 2) Racket symbols represent variables (for example, 'x, 'y, 'z2) and
;    are valid Boolean expressions

; 3) If exp1 and exp2 represent Boolean expressions, then
;    (bnot exp1) represents the NOT of exp1
;    (band exp1 exp2) represents the AND of exp1 and exp2
;    (bor exp1 exp2) represents the OR of exp1 and exp2

; Some examples of Boolean expressions follow.

(define exp0 (bnot 0))
(define exp1 (bor 'x 'y))
(define exp2 (band 'y 'z))
(define exp3 (band 'w (bor (bnot 'x) 0)))
(define exp4 (bor 'x (bnot 'x)))
(define exp5 (band (bor 'x (bnot 'y)) (bnot (band 0 'z))))

; ****************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (boolean-exp? exp)
; (type-of exp)

; (boolean-exp? exp) takes an arbitrary Racket value exp
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; (type-of exp)
; that takes a Boolean expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.

; Recall that (number? exp) tests whether exp is a number,
; and (symbol? exp) tests whether exp is a symbol, and the
; structs bnot, band, and bor have type predicates bnot?, band?, and bor?

; Examples
;> (boolean-exp? 0)
;#t
;> (boolean-exp? 2)
;#f
;> (boolean-exp? exp0)
;#t
;> (boolean-exp? (band "hi" 'c))
;#f
;> (boolean-exp? (band 'x (bor 0 (bnot 1))))
;#t
;> (boolean-exp? (band 'x (bor 0 (bnot #t))))
;#f
;> (type-of 0)
;'constant
;> (type-of 'hi)
;'variable
;> (type-of (bnot (band 'x 0)))
;'not
;> (type-of (bor (band 'x 'y) (band 'x 'z)))
;'or
;> (type-of (band (bor 0 1) (bnot 'x)))
;'and
; ****************************************************************

(define (boolean-exp? exp)
  (cond
    [(empty? exp) #f]
    [(symbol? exp ) #t]
    [(equal? exp 1) #t]
    [(equal? exp 0) #t]
    [(bnot? exp) (boolean-exp? (bnot-arg exp))]
    [(band? exp) (and(boolean-exp? (band-arg1 exp)) (boolean-exp?(band-arg2 exp)))]
    [(bor? exp) (and(boolean-exp? (bor-arg1 exp)) (boolean-exp?(bor-arg2 exp)))]
    [else
     #f]
  )
  )

(define (type-of exp)
  (cond
    [(band? exp) 'and]
    [(bor? exp) 'or]
    [(bnot? exp) 'not]
    [(number? exp) 'constant]
    [else
     'variable
     ]
  )
)

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (all-vars exp)

; that takes a Boolean expression exp 
; and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)

; Hint: selectors, type-of and deep recursion on the structure 
; of Boolean expressions.  Note that there is a Racket procedure
; remove-duplicates.

; Examples
;> (all-vars 0)
;'()
;> (all-vars (bnot (band 'x (band 'y (bor 'x 'z)))))
;'(x y z)
;> (all-vars (band 1 (bor 0 (bnot 'u))))
;'(u)
;> (all-vars (band (band 'x 'y) (band 'y 'x)))
;'(x y)
;> (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c))
;'(c b a)
;> 
; ****************************************************************

(define (all-vars exp)
  (remove-duplicates(all-varshelp exp '())) 
  )

; does the bulk of the recusrion for the method, recursively goes through the expressions and adds the variables to a list
; ex: (all-varshelp (band 'a 'b) '()) --> (a b)
  (define (all-varshelp exp list)
     (cond
       [(empty? exp) list]
       [(band? exp) (append(all-varshelp (band-arg1 exp)list) (all-varshelp (band-arg2 exp)list))]
       [(bor? exp) (append(all-varshelp (bor-arg1 exp)list) (all-varshelp (bor-arg2 exp)list))]
       [(bnot? exp) (all-varshelp (bnot-arg exp) list)]
       [(symbol? exp) (cons exp list)]
       [else list]
       
    ))
  

; ****************************************************************
; We represent an environment as a table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

; (eval-in-env exp env)

; that takes a Boolean expression exp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env exp env) should
; return the symbol: 'unspecified-variable.
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions
; along with argument selectors and type-of.

; Examples
;> (eval-in-env 1 environ1)
;1
;> (eval-in-env (bor 0 0) '())
;0
;> (eval-in-env 'x environ1)
;0
;> (eval-in-env 'x environ2)
;1
;> (eval-in-env (bnot 'z) environ1)
;1
;> (eval-in-env (bor 'y (bnot 'x)) environ2)
;0
;> (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2)
;1
;> (eval-in-env exp5 environ1)
;0
;> (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1)))
;'unspecified-variable
;>
; ****************************************************************

;recursively iterates through the expression and checks it against the enviroment
; ex: (eval-in-envhelp 'x ('x 1)) --> 1
 (define (eval-in-envhelp exp env)
   (cond
    [(equal? (type-of exp) 'constant) exp]
    [(equal? (type-of exp) 'variable) (lookup exp env)] 
    [(equal? (type-of exp) 'and) (bandhelp (eval-in-envhelp (band-arg1 exp) env) (eval-in-envhelp (band-arg2 exp) env))]
    [(equal? (type-of exp) 'or) (borhelp (eval-in-envhelp (bor-arg1 exp) env) (eval-in-envhelp (bor-arg2 exp) env))]
    [(equal? (type-of exp) 'not) (bnothelp (eval-in-envhelp(bnot-arg exp) env))]
    )   
   )

; checks if the expression is a 1 and then nots it, otherwise returning 1
; ex: (bnothelp 1) --> 0 
(define (bnothelp exp)
  (if (= exp 1)
      0
      1)
  )
; checks if two expressions are both 1 otherwise returns 0
; ex: (bandhelp 0 1) --> 0
; ex: (bandhelp 1 1) --> 1 
(define (bandhelp exp1 exp2)
  (if (and (equal? exp1 1) (equal? exp2 1)) 1
      0)
  )
; checks if both expressions are 0 otherwise returns 1
; ex: (borhelp 0 0) --> 0
; ex: (bandhelp 1 0) --> 1
(define (borhelp exp1 exp2)
  (if (and (equal? exp1 0) (equal? exp2 0)) 0
      1)
  )
(require racket/trace)
;(trace eval-in-envhelp)
 
(define (eval-in-env exp env)
  (cond          
    [(not (andmap (lambda (x) (lookup x env)) (all-vars exp))) 'unspecified-variable]
    [else
     (eval-in-envhelp exp env)
     ]
     
  ) 
 )




;(error "eval-in-env not defined yet")
  
; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each combination of n 0's and 1's,
; whose key is a list of n 0's and 1's and whose value is the value (0 or 1)
; of the function represented by the truth table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the expressions
; (bnot 'x), (band 'x 'y), (bor (bnot 'a) 'b) 
; and the function that is the XOR of 'u and 'v.

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 
(define tt-imp (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (all-combs n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as binary numbers, 
; should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-combs 2), what needs to happen to it
; to give the correct answer for (all-combs 3)?
; (This may remind you of power-set from assignment #1.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
;> (all-combs 0)
;'(())
;> (all-combs 1)
;'((0) (1))
;> (all-combs 2)
;'((0 0) (0 1) (1 0) (1 1))
;> (all-combs 3)
;'((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
;> 
; ****************************************************************


(define (all-combs n)
  (reverse (all-combshelp n))
)

;does the bulk of the recursive work, returns the combinations of the truth table
; ex: (all-combshelp 1) --> '((0) (1))
(define (all-combshelp n)
  (if (= n 0) 
      '(())
      (let* (
             (combs (all-combshelp (- n 1)))
             (first (map (lambda (x) (cons 1 x)) combs))
             (second (map (lambda (x) (cons 0 x)) combs)))
        (append first second)
        )
      )
  )

;(define (power-set lst)
  ; (if (empty? lst) (list '())
;     (append (power-set (cdr lst)) (map (lambda (x) (cons(car lst) x)) (power-set (cdr lst))))
 ;     )
 ;)

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (truth-table exp)

; that takes a Boolean expression exp and returns the truth table for exp
; where the variables for the table are extracted from exp using all-vars, 
; and the function value for each row is obtained by evaluating exp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:
;> (truth-table exp0)
;(tt '() (list (entry '() 1)))
;> (truth-table exp1)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))
;> (truth-table exp5)
;(tt
; '(x y z)
; (list
;  (entry '(0 0 0) 1)
;  (entry '(0 0 1) 1)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 1)
;  (entry '(1 1 1) 1)))
;> 
; ****************************************************************

;makes the enviroment
; ex: (helper '(a) ()) --> '(a)
(define (helper vars lst)
  (cond
    [(empty? vars) '()]
    [else
     (cons (entry (car vars) (car lst)) (helper (cdr vars) (cdr lst)))
     ]
  )
  )

(define (truth-table exp)
  (tt (all-vars exp) (map (lambda (x) (entry x (eval-in-env exp (helper (all-vars exp) x)))) (all-combs (length (all-vars exp)))))  
  )  

; ****************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (satisfiable? exp)
; (equivalent? exp1 exp2)

; (satisfiable? exp)
; takes one Boolean expression exp and
; returns #t if exp is satisfiable and #f otherwise.

; (equivalent? exp1 exp2)
; takes two Boolean expressions exp1 and exp2 
; and returns #t if they are equivalent and #f if they
; are not equivalent.

; A Boolean expression is satisfiable if there exists an environment
; in which its value is 1.  Two Boolean expressions are equivalent
; if for every environment which assigns values to all the variables
; in either expression, they have the same value in that environment.

; One possibility might be to use satisfiable? to help implement equivalent?

; These procedures will be tested on expressions with few enough
; variables that generating truth tables will be a feasible approach.

; Examples:
;> (satisfiable? 0)
;#f
;> (satisfiable? 1)
;#t
;> (satisfiable? (band 'x (band 'y 'z)))
;#t
;> (satisfiable? (band 'x (band 'y (bnot 'y))))
;#f
;> (satisfiable? (band (bor 'x (bnot 'y)) 0))
;#f
;> (equivalent? 0 (band 'a (bnot 'a)))
;#t
;> (equivalent? 0 'a) 
;#f
;> (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y))))
;#t
;> (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z)))
;#t
;> 
; ****************************************************************

(define (satisfiable? exp)
  (cond
    [(band? exp) (and(satisfiable? (band-arg1 exp)) (satisfiable? (band-arg2 exp)))]
    [(bor? exp) (or(satisfiable? (bor-arg1 exp)) (satisfiable? (bor-arg2 exp)))]
    [(bnot? exp) (not(satisfiable? (bnot-arg exp)))]
    [(symbol? exp) #t]
    [(equal? exp 0) #f]
    [(equal? exp 1) #t]
    [else
     #f
     ]
    ))

(define (equivalent? exp1 exp2)
  (if (eq? (if (satisfiable? exp1)
               #t 
               #f)
       (if (satisfiable? exp2) #t
                  #f))
      #t
      #f
       ))  
  ;add expressions together and then call satifiable on each line of the truth table, if it ever returns 1 return false 
 ; (error "equivalent? not defined yet")
 ; )

; ****************************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method.
; Please include comments explaining your method
; in either case.

;Examples
;>  (boolean-exp? (find-exp tt-and))
;#t
;>  (equivalent? (find-exp tt-and) (band 'x 'y))
;#t
;>  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) 
;#t
;>  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v)))
;#t
;>  (boolean-exp? (find-exp tt-f1))
;#t
;>
; not for x
; and for x and y
; or for sums across rows 
; ****************************************************************
; clears the truth table to be only outputs with 1 (set of rows keep the rows where the output is 1)
; not -helper fucntion: takes a single variable and a single num, eitehr spits out og variable or the not of that variable
; and-helper: now looking at a row, go through and and together all the variable and makes sure the variables get properly notted,
; takes names of the variables and the output in that enviroment
; or-helper: vars (list of entries), set up ors between every row, and feed in current entry row into and helper

; nots the 0s in the row and the ands the row together
; ex: (exphelper (a b) (0 0) )--> (1 1) 
(define (exphelper vars row)
  (cond
    [(empty? row) 1]
    [(= (car row) 0) (band (bnot (car vars)) (exphelper (cdr vars) (cdr row)))]
    [else
     (band (car vars) (exphelper (cdr vars) (cdr row)))
     ])
  )

(define (find-exp trutht)
  (cond
    [(empty? (tt-rows trutht)) 0]
    [(= 1 (entry-value (car(tt-rows trutht)))) (bor (exphelper (tt-vars trutht) (entry-key(car(tt-rows trutht)))) (find-exp (tt (tt-vars trutht) (cdr(tt-rows trutht)))))]
    [else
     (find-exp (tt (tt-vars trutht) (cdr(tt-rows trutht))))
     ]
  )
  ) 
 
; ****************************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (substitute-in exp sub-table)

; that takes a Boolean expression exp and a table sub-table each of whose
; entries has a key that is a variable and a value that is a
; Boolean expression, and returns the Boolean expression obtained
; by replacing every occurrence in exp of a variable in the sub-table with
; the corresponding Boolean expression.

; Hint: type-of and deep recursion on the structure of Boolean expressions.

; Examples
;> (substitute-in 0 (list (entry 'x 1)))
;0
;> (substitute-in 'x (list (entry 'x 1)))
;1
;> (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0)))
;(band (bnot 'z) 0)
;> (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1))))
;(band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y))
;> 
; ****************************************************************

(define (substitute-in exp sub-table)
  (cond
    [(empty? exp) 0]
    [(equal? (type-of exp) 'constant) exp]
    [(equal? (type-of exp) 'variable)
     (if (equal? (lookup exp sub-table) #f) exp
     (lookup exp sub-table))] 
    [(equal? (type-of exp) 'and) (band (substitute-in (band-arg1 exp) sub-table) (substitute-in (band-arg2 exp) sub-table))]
    [(equal? (type-of exp) 'or) (bor (substitute-in (bor-arg1 exp) sub-table) (substitute-in (bor-arg2 exp) sub-table))]
    [(equal? (type-of exp) 'not) (bnot (substitute-in(bnot-arg exp) sub-table))]
    )   
   )

  

; ****************************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (match exp pat)

; that takes two Boolean expressions exp and pat and
; attempts to "match" the expression exp to the pattern pat.
; That is, it tries to find a table sub-table (as in the preceding
; problem) such that (substitute-in pat sub-table) returns
; the expression exp.  If this is not possible, it should return #f.

; Examples
;> (match 1 1)
;'()
;> (match 0 'x)
;(list (entry 'x 0))
;> (match 'z 0)
;#f
;> (match 'z 'x)
;(list (entry 'x 'z))
;> (match (band 'y 'x) 'x)
;(list (entry 'x (band 'y 'x)))
;> (match (bnot (band 'a 'b)) (bnot 'z))
;(list (entry 'z (band 'a 'b)))
;> (match (bor (band 'x 'y) 0) (bor 'a 0))
;(list (entry 'a (band 'x 'y)))
;> (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c)))
;(list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y))
;> (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c))   
;#f
;> (match (band 'x 'y) (band 'a 'a))
;#f
;> (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a)))
;(band (bor 0 1) (bor 0 1))
;>
;1. constant and constant (both are 1s or both are 0s)
; if you dont need to write anything, append an empty list '()
;2. band and band
;3. bor and bor
; 4. bnot and bnot
;5. varible and any boolean expression --> set varible to be an entry and return that entry in a lsit
;6. all other cases cannot match so return a #f 
; ****************************************************************

;checks all the different conditions of possible expressions and patterns that could be matched together
; ex: (matchhelp 1 1) '()
(define (matchhelp exp pat)
  (cond
    [(and (equal? exp 1) (equal? pat 1)) '()]
    [(and (equal? exp 0) (equal? pat 0)) '()]
    [(and (band? exp) (band? pat)) (append (matchhelp (band-arg1 exp) (band-arg1 pat)) (matchhelp (band-arg2 exp) (band-arg2 pat)))]
    [(and (bor? exp) (bor? pat)) (append (matchhelp (bor-arg1 exp) (bor-arg1 pat)) (matchhelp (bor-arg2 exp) (bor-arg2 pat)))]
    [(and (bnot? exp) (bnot? pat)) (matchhelp (bnot-arg exp) (bnot-arg pat))]
    [(equal? (type-of pat) 'variable) (list (entry pat exp))]
    [else
     (list #f)]
  ))




(define (match exp pat)
  (let*
    ([lst (matchhelp exp pat)]
    [final (remove-duplicates lst)])
  (cond
    [(member #f lst) #f]
    [(unique-keys? final) final]
    [else
     #f]))
  )
                  
; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (testold name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    '***OK***
			    '***X***)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))


(test 'lookup  (lookup "first" test1) 1)
(test 'lookup  (lookup "third" test1) #f)
(test 'lookup  (lookup 'z test2) 1)
(test 'unique-keys?  (unique-keys? test1) #t)
(test 'unique-keys?  (unique-keys? test2) #f)


(test 'boolean-exp?  (boolean-exp? 0) #t)
(test 'boolean-exp?  (boolean-exp? 2) #f)
(test 'boolean-exp?  (boolean-exp? exp0) #t)
(test 'boolean-exp?  (boolean-exp? (band "hi" 'c)) #f)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot 1)))) #t)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot #t)))) #f)
(test 'type-of  (type-of 0) 'constant)
(test 'type-of  (type-of 'hi) 'variable)
(test 'type-of  (type-of (bnot (band 'x 0))) 'not)
(test 'type-of  (type-of (bor (band 'x 'y) (band 'x 'z))) 'or)
(test 'type-of  (type-of (band (bor 0 1) (bnot 'x))) 'and)

(test 'all-vars (all-vars 0) '())
(test 'all-vars (all-vars (bnot (band 'x (band 'y (bor 'x 'z))))) '(x y z))
(test 'all-vars (all-vars (band 1 (bor 0 (bnot 'u)))) '(u))
(test 'all-vars (all-vars (band (band 'x 'y) (band 'y 'x))) '(x y))
(test 'all-vars (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c)) '(c b a))


(test 'eval-in-env (eval-in-env 1 environ1) 1)
(test 'eval-in-env (eval-in-env (bor 0 0) '()) 0)
(test 'eval-in-env (eval-in-env 'x environ1) 0)
(test 'eval-in-env (eval-in-env 'x environ2) 1)
(test 'eval-in-env (eval-in-env (bnot 'z) environ1) 1)
(test 'eval-in-env (eval-in-env (bor 'y (bnot 'x)) environ2) 0)
(test 'eval-in-env (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2) 1)
(test 'eval-in-env (eval-in-env exp5 environ1) 0)
(test 'eval-in-env (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1))) 'unspecified-variable)


(test 'all-combs (all-combs 0) '(()))
(test 'all-combs (all-combs 1) '((0) (1)))
(test 'all-combs (all-combs 2) '((0 0) (0 1) (1 0) (1 1)))
(test 'all-combs (all-combs 3) '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))


(test 'truth-table (truth-table exp0) (tt '() (list (entry '() 1))))
(test 'truth-table (truth-table exp1) 
      (tt '(x y)
	  (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1))))
(test 'truth-table (truth-table exp5) (tt
				       '(x y z)
				       (list
					(entry '(0 0 0) 1)
					(entry '(0 0 1) 1)
					(entry '(0 1 0) 0)
					(entry '(0 1 1) 0)
					(entry '(1 0 0) 1)
					(entry '(1 0 1) 1)
					(entry '(1 1 0) 1)
					(entry '(1 1 1) 1))))



(test satisfiable?  (satisfiable? 0) #f)
(test satisfiable? (satisfiable? 1) #t)
(test satisfiable?  (satisfiable? (band 'x (band 'y 'z))) #t)
(test satisfiable? (satisfiable? (band 'x (band 'y (bnot 'y)))) #f)
(test satisfiable?  (satisfiable? (band (bor 'x (bnot 'y)) 0)) #f)
(test equivalent?  (equivalent? 0 (band 'a (bnot 'a))) #t)
(test equivalent?  (equivalent? 0 'a) #f)
(test equivalent?  (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y)))) #t)
(test equivalent? (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z))) #t)


(test 'find-exp  (boolean-exp? (find-exp tt-and)) #t)
(test 'find-exp  (equivalent? (find-exp tt-and) (band 'x 'y)) #t)
(test 'find-exp  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) #t)
(test 'find-exp  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v))) #t)
(test 'find-exp  (boolean-exp? (find-exp tt-f1)) #t)

(test 'substitute-in (substitute-in 0 (list (entry 'x 1))) 0)
(test 'substitute-in (substitute-in 'x (list (entry 'x 1))) 1)
(test 'substitute-in (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0))) (band (bnot 'z) 0))
(test 'substitute-in (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1)))) (band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y)))


(test 'match (match 1 1) '())
(test 'match (match 0 'x) (list (entry 'x 0)))
(test 'match (match 'z 0) #f)
(test 'match  (match 'z 'x) (list (entry 'x 'z)))
(test 'match (match (band 'y 'x) 'x) (list (entry 'x (band 'y 'x))))
(test 'match  (match (bnot (band 'a 'b)) (bnot 'z)) (list (entry 'z (band 'a 'b))))
(test 'match (match (bor (band 'x 'y) 0) (bor 'a 0)) (list (entry 'a (band 'x 'y))))
(test 'match (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c))) (list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y)))
(test 'match (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c)) #f)
(test 'match (match (band 'x 'y) (band 'a 'a)) #f)
(test 'match (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a))) (band (bor 0 1) (bor 0 1)))



; **************** end of hw #4 *********************************