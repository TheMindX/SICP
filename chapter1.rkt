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
  (define mod (modulo base n))
  (cond
    ((eq? mod 0) 0)
    ((eq? p 1) (common.mod base n))
    ((even? p) 
     (expmod (* mod mod) (/ p 2) n) )
    (else 
     (modulo (* mod (expmod base (- p 1 ) n)) n))))


;(expmod 2 51 15)

;ferma-test n
;素数检测
(define (ferma-test x)
  (define times 25)
  (define (randa) (+ 2 (random (- x 3))))
  (define (testonce)
    (define ra (randa))
    (define em (expmod ra x x))
    ;(common.println ra)
    ;(common.println em)
    (eq? em ra))
  (define (testn i)
    (if (eq? i times) 
        true
        (if (testonce) 
            (testn (+ i 1))
            false)))
  
  (cond ((< x 2) #f)
        ((eq? x 2) #t)
        ((eq? x 3) #t)
        (else (testn 1))))
  
;(ferma-test 683333333)

;common sum sigma
(define (mysum a b termf nextf)
  (if (> a b)
      0
      (begin
        ;(common.println (termf a))
        (+ (termf a) (mysum (nextf a) b termf nextf)))))

;通用的累积 
(define (accum a b mapf unit combinef nextf filterf)
  (cond ((> a b) unit)
        ((not (filterf a)) (accum (nextf a) b mapf unit combinef nextf filterf))
        (else (combinef (mapf a) (accum (nextf a) b mapf unit combinef nextf filterf)))))
      
;(accum 1 100 identity 0 + (lambda (x) (+ 1 x)) (const true))

(define (mypi times)
  (define (term i)
    (define a (+ 1 (* 4 (- i 1))))
    (define b (+ a 2))
    (/ 1 (* a b)))
  (define (next i)
    (+ i 1))
  (define pi/8
    (mysum 1 times term next))
  (* pi/8 8))

;(exact->inexact (mypi 1120))


;integral
(define (integral fx a b dx)
  (letrec
      ((iter (lambda (i c)
               (if (>= i b)
                   c
                   (iter (+ dx i) (+ c (* dx (fx i))))))))
    (iter a 0)))


;(integral (lambda (x) (* x x x)) 0 1 0.001)

(define (integral1 fx a b dx)
  (mysum 0 1 (lambda (x) (* dx (fx x)))  (lambda (x) (+ x dx))))

(integral1 common.cube 0 1 0.001)

(define (simpsonsrlue fx a b divn) 
  (define n (if (common.even? divn) 
                divn
                (+ 1 divn)))
  (define h (/ (- b a) n))
  (define (kh k) (cond ((eq? k 0) 1)
                       ((eq? k n) 1)
                       ((even? k) 2)
                       (else 4)))
  (define (yk k) (fx (+ a (* k h))))
  (define (iter i c)
    (if (> i n)
        c
        (iter (+ i 1) (+ c (* (kh i) (yk i))))))
  ;(common.println n)
  ;(common.println h)
  (* (/ h 3) (iter 0 0)))
                     

;(integral (lambda (x) (/ 1 x)) 1 2 0.001)
;(exact->inexact (simpsonsrlue (lambda (x) (/ 1 x)) 1 2 1000))
;TODO fix integral1



;求a~b中所有素数之和
;(accum 1 10000 identity 0 + (lambda (x) (+ x 1)) ferma-test)









