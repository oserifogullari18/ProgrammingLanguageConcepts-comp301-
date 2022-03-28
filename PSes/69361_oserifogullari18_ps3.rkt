(module ps3 mzscheme
  
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROBLEM 1 Part A | Write your answer below here as a comment
  ;
  ;
  ;
  ;
  ;
  ;
  ;
  ;; PROBLEM 1 Part B
  ;; Unary Representation | We added a -u suffix so that both Unary and BigNum can be tested at once.

  (define create-u
    (lambda (n) (if(zero? n) '() (cons #t (create-u (- n 1))))))

  (define is-zero-u?
    (lambda (lst) (if (null? lst) #t #f)))

  (define predecessor-u
    (lambda (lst) (if (null? lst) "error" (cdr lst))))

  ;; BigNum Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  (define create-b
    (lambda (n i) (if (zero? n) '() (cons (modulo n i) (create-b (quotient n i) i)))))
  (define is-zero-b?
    (lambda (lst) (is-zero-b? lst) (if (null? lst) #t #f)))

  (define predecessor-b
    (lambda (lst i) (if (null? lst) "error" (if (= 0 (car lst)) (if (null? (cdr lst)) "error" (cons (- i 1) (predecessor-b (cdr lst)))) (cons (- (car lst) 1) (cdr lst))))))


  ;; PROBLEM 2 Part A
  (define count-free-occurrences
    (lambda (sym exp) (occurrences-free sym exp 0)))
  (define (occurrences-free var exp i) (cond((symbol? exp) (if (equal? var exp) (+ 1 i) i)) ((equal? (car exp) 'lambda) (if (equal? var (car (cadr exp))) i (+ i (occurrences-free var (caddr exp) 0)))) (else (+ i (+ (occurrences-free var (car exp) 0) (occurrences-free var (cadr exp) 0))))))

  ;; PROBLEM 2 Part B (optional)
  (define product
    (lambda (sos1 sos2)()))
  
   ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  ;;; If you don't implement the functions in order and want to test as you go, you can comment out the corresponding tests,
  ;;; otherwise, DrRacket will raise errors.
  ;; PROBLEM 1 TESTS
  ;;; For unary representation
  (display "Unary Tests\n")
  (equal?? (create-u 4) '(#t #t #t #t)) ; should return '(#t #t #t #t)
  (equal?? (is-zero-u? '(#t #t #t)) #f) ; should return #f
  (equal?? (is-zero-u? '()) #t) ; should return #t
  (equal?? (predecessor-u '(#t #t #t)) '(#t #t)) ; should return '(#t #t)
  (equal?? (predecessor-u '()) 'error-only-positive-numbers)
  (newline)

  ;;; For BigNum representation
  (display "\nBigNum Tests\n")
  (equal?? (create-b 15 4) '(3 3)) ; should return '(3 3)
  (equal?? (is-zero-b? (create-b 0 4)) #t) ; should return #t
  (equal?? (is-zero-b? (create-b 5 4)) #f) ; should return #f
  (equal?? (predecessor-b (create-b 31 4) 4) '(2 3 1)) ; should return '(2 3 1)
  (equal?? (predecessor-b (create-b 64 4) 4) '(3 3 3 0)) ; should return '(3 3 3 0)
  (equal?? (predecessor-b (create-b 0 4) 4) 'error-only-positive-numbers) ; should return error

  (newline)

  ;; PROBLEM 2 Part A TESTS
  (display "\nCount Free Occurences Tests\n")
  (equal?? (count-free-occurrences 'x 'x) 1) ;1
  (equal?? (count-free-occurrences 'x 'y) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (x) (x y))) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (y) (x x))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (xx) x) (x y))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (x) (y x)) (lambda (y) (x (lambda (z) x))))) 2) ;2

  ;; PROBLEM 2 Part B TESTS (Optional)
  (display "\nCartesian Product Tests\n")
  (equal?? (product '(x y) '(a b)) '((x a) (x b) (y a) (y b)))
  (equal?? (product '() '(a b)) '())
  
)