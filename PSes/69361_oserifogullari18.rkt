#lang eopl
  
;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
;; PROBLEM 1

;; Part A
(define (empty-env) (list 'empty-env))

(define (extend-env var val env)
  (list 'extend-env (list var val) env))

(define (apply-env env search-var)
  (cond
     ((eqv? (car env) 'empty-env) (display "no binding found"))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadadr env))
            (saved-val (caadr env))
            (saved-env (caddr env)))
        (if (eqv? search-var saved-val)
            saved-var
            (apply-env saved-env search-var))))
     (else
     (display "invalid env"))))
  
(define (empty-env? env)
  (if (eqv? (car env) 'empty-env) #t #f))

(define (has-binding? env search-var)
  (cond
    ((eqv? env 'empty-env) #f)
    ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadadr env))
            (saved-val (caadr env))
            (saved-env (caddr env)))
        (if (eqv? search-var saved-val)
            #t
            (has-binding? saved-env search-var))))
     (else
      #f )))

;; Part B
  
(define (extend-env* var-list val-list env)
  (if (null? var-list)
      env
      (extend-env* (cdr var-list) (cdr val-list) (list 'extend-env (list (car var-list) (car val-list)) env))))


;; Part C

(define (empty-rib-env) (list 'empty-rib-env))

(define (extend-rib-env* var-list val-list env)
   (if (null? var-list)
      'empty-rib-env
      (list 'extend-rib-env (list var-list val-list))))
;
(define (apply-rib-env env search-var)
  (cond
     ((eqv? (car env) 'empty-rib-env) (display "no binding found"))
     ((eqv? (car env) 'extend-rib-env))
      ((let ((saved-var (cadar env))
            (saved-val (cddar env))
            (saved-env (cadr env)))
        (if (eqv? search-var saved-val)
            saved-var
            (apply-rib-env saved-env search-var))))
     (else
     (display "invalid env"))))

;; Problem 2

;; Part A

  
(define-datatype stack stack?
  (empty-stack)
  (a-stack
   (top integer?)
   (rest stack?)))
  
;; Part B

(define (add val stk)
  (a-stack val stk))
;
(define (top stk)
  (cases stack stk
    (empty-stack () 'empty-stack)
    (a-stack (top rest) top)))
;
(define (pop stk)
  (cases stack stk
    (empty-stack () 'empty-stack)
    (a-stack (top rest) rest)))
;
(define (empty? stk)
  (cases stack stk
    (empty-stack () #t)
     (else #f)))

;; Part C

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; Part D

(define (bintree-to-list tree)
  (cases bintree tree
    (leaf-node (num)
               (list 'leaf-node num))
    (interior-node (key left right)
                   (list 'interior-node key (bintree-to-list left) (bintree-to-list right)))))
;
(define (sum-leafs tree)
  (cases bintree tree
    (leaf-node (num)
               num)
    (interior-node (key left right)
                   (+ (sum-leafs left) (sum-leafs right)))))





  
;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't worry about the function below, we included it to test your implemented functions and display the result in the console
;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
(define-syntax equal??
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (eopl:printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (eopl:printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
;;; If you don't implement the functions in order and want to test as you go, you can comment out the corresponding tests,
;;; otherwise, DrRacket will raise errors.
;; PROBLEM 1 TESTS
(display "\nProblem 1 Tests\n")
(display "\nPart A\n")

(equal?? (extend-env 'a 5 (empty-env)) '(extend-env (a 5) (empty-env)))
(equal?? (extend-env 'a 5 (extend-env 'b 2 (empty-env)))
         '(extend-env (a 5) (extend-env (b 2) (empty-env))))
(equal?? (apply-env (extend-env 'a 5 (extend-env 'b 2 (empty-env))) 'a) 5)
(equal?? (apply-env (extend-env 'a 5 (extend-env 'b 2 (empty-env))) 'b) 2)
(equal?? (empty-env? (empty-env)) #t)
(equal?? (empty-env? (extend-env 'a 5 (empty-env))) #f)
(equal?? (has-binding? (extend-env 'a 5 (extend-env 'b 2 (empty-env))) 'b) #t)
(equal?? (has-binding? (extend-env 'a 5 (extend-env 'b 2 (empty-env))) 'c) #f)

(display "\nPart B\n")
  
(equal?? (extend-env* '(a b c d) (list 2 4 5 7) (empty-env))
         '(extend-env (d 7) (extend-env (c 5) (extend-env (b 4) (extend-env (a 2) (empty-env))))))
(equal?? (extend-env* '(a b) (list 2 4) (extend-env* '(c d) (list 5 7) (empty-env)))
         '(extend-env (b 4) (extend-env (a 2) (extend-env (d 7) (extend-env (c 5) (empty-env))))))

(display "\nPart C\n")

(define test-rib-env (extend-rib-env* '(a b) (list 2 4)
                                      (extend-rib-env* '(c d e) (list 3 5 8)
                                                       (extend-rib-env* '(f) (list 1)
                                                                        (empty-rib-env)))))

(equal?? test-rib-env '(extend-rib-env ((a b) (2 4)) (extend-rib-env ((c d e) (3 5 8)) (extend-rib-env ((f) (1)) (empty-rib-env)))))
(equal?? (apply-rib-env test-rib-env 'c) 3)
(equal?? (apply-rib-env test-rib-env 'a) 2)
(equal?? (apply-rib-env test-rib-env 'e) 8)
(equal?? (apply-rib-env test-rib-env 'f) 1)
(equal?? (apply-rib-env test-rib-env 'b) 4)
(equal?? (apply-rib-env test-rib-env 'd) 5)

(display "\nProblem 2 Tests\n")
(display "\nPart B\n")

(define test-stack (add 4 (add 6 (add 3 (empty-stack)))))

(equal?? (empty? (empty-stack)) #t)
(equal?? (empty? (add 5 (empty-stack))) #f)
(equal?? (top test-stack) 4)
(equal?? (top (pop test-stack)) 6)
(equal?? (top (add 9 test-stack)) 9)
(equal?? (top (pop (pop test-stack))) 3)

(display "\nPart D\n")

(define test-bintree (interior-node 'a
                                    (interior-node 'b (leaf-node 6) (leaf-node 9))
                                    (interior-node 'c (leaf-node 2)
                                                   (interior-node 'd (leaf-node 3) (leaf-node 5)))))
(equal?? (bintree-to-list test-bintree) '(interior-node a (interior-node b (leaf-node 6) (leaf-node 9)) (interior-node c (leaf-node 2) (interior-node d (leaf-node 3) (leaf-node 5)))))
(equal?? (sum-leafs test-bintree) 25)


