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


(exact->inexact (my_sq 2))


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


;(fast_expt 2 31)

;;; sin 
(define (mysin a)
  (define (cube x) (* x x x))
  (if (< a 0.001)
      a
      (let ((sindiv3 (mysin (/ a 3))))
        (- (* 3 sindiv3) (* 4 (cube sindiv3))))))
      

(exact->inexact (mysin 5) )