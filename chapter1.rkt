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
  (if (eq? n 1) b
   (if (common.even? n)
      (common.square (fast_expt b (/ n 2)))
      (* b (fast_expt b (- n 1))))))

;(fast_expt 3 3)


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

;(gcd 16 8)

;find primarys 
(define (primary_slow? n)
  (define (smallest_div n)
    (define (find_small i)
      (define sq (my_sq n))
      (cond 
        ((> i  sq) n)
        ((eq? (common.mod n i) 0) i)
        (else (find_small (+ i 1)))))
    (find_small 2))
  (eq? (smallest_div n) n))


;(primary_slow? 19)

;todo 筛法 
#|
let primary?(n):
	plist = 
		var i = 2;
		yield i;
		loop:
			i = i+1
			if is_primary?(i):
				yield i
			is_primary?(i) = 
				sq = sqare(i)
				for p in plist:
					if p > sq:
						return true
					else:
						if mod(i p) == 0:
							return false
				
	for p in plist:
		if p == n:
			return true
		if p > n:
			return false

 |#


;对一个数的power 取模
(define (expmod base p n)
  (define mod (common.mod base n))
  (cond
    ((eq? mod 0) 0)
    ((eq? p 1) (common.mod base n))
    ((common.even? p) 
     (expmod (common.square mod) (/ p 2) n) )
    (else 
     (common.mod (* mod (expmod base (- p 1 ) n)) n))))
 
  
;(expmod 2 51 15)

;ferma-test n











