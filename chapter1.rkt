#lang racket

(require "common.rkt")

;开方 
(define (my_sq x)
	(define (sq_iter guess x)
	  (if (goodenough guess x)
		  guess
		  (sq_iter (imporve guess x) x)))
	(define (goodenough guess x)
	  (if (< (abs (- (* guess guess) x)) 0.00000000000000001)
		  #true
		  #false))
	(define (imporve guess x)
	  (/ (+ guess (/ x guess) ) 2))
    (sq_iter (/ x 2) x))


;(exact->inexact (my_sq 2))


;;;;power
(define (mypower n)
  (letrec ((mypower_iter
            (lambda (i c)
             (if (eq? i (+ 1 n))
               c
               (mypower_iter (+ i 1) (* c i))))))
    
    (mypower_iter 1 1)) )

;(mypower 5)
    
;named let
(define (mypower1 n)
  (let loop ((i 1) (c 1))
    (if (eq? i n)
        (* i c)
        (loop (+ i 1) (* c i)))))
  
;(mypower1 5)


;快速exp乘 
(define (fast_expt b n)
  (define (square k) (* k k) )
  (define (reminder a b)
    (if (< a b)
        a
        (reminder (- a b) b)))
  (define (myeven? n)
    (eq? (reminder n 2) 0))
  
  (if (eq? n 1) b
   (if (myeven? n)
      (square (fast_expt b (/ n 2)))
      (* b (fast_expt b (- n 1))))))

;iter impl, just like fold,
(define (fast_exp2 m n)
  (let loop((m1 m) (n1 n) (rem 1) )
    (let* ((mod (common.mod n1 2))
          (div (/ (- n1 mod) 2))
          (remIter (if (eq? mod 1) (* rem m1) rem)))
      (if (eq? n1 1)
          (* m1 rem)
          (loop (common.square m1) div remIter)))))


;(fast_exp2 2 31)

(define (fast_mul a b)
  (let loop ((a1 a) (b1 b) (count 0))
    (let* ((mod (common.mod b1 2))
           (div (/ (- b1 mod) 2))
           (count1 (if (eq? mod 0) count (+ count a1))))
      (if (eq? b1 1)
          count1
          (loop (+ a1 a1) div count1)))))
          
;(fast_mul 5 1024)

;;; sin 
(define (mysin a)
  (define (cube x) (* x x x))
  (if (< a 0.001)
      a
      (let ((sindiv3 (mysin (/ a 3))))
        (- (* 3 sindiv3) (* 4 (cube sindiv3))))))
      

;(exact->inexact (mysin 5) )

;note fast fib
(define (fib n)
  (define (iter fir sec iterTime)
    (if (eq? iterTime n)
        sec
        (iter sec (+ fir sec) (+ iterTime 1))))
  (iter 0 1 2))

;(fib 115)
    
  
;gcd
(define (gcd m n)
  (cond 
    ((< m n) (gcd n m))
    ((eq? m n) m)
    ((eq? n 0) m)
    (else (gcd n (common.mod m n)))))

(gcd 16 8)

