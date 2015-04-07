#lang racket

(define (fac n)
  (letrec ((fac_iter
           (lambda (i c)
           (if (eq? i (+ 1 n))
               c
               (fac_iter (+ i 1) (* c i)))))
   )
    
    (fac_iter 1 1))  
  )


(fac 5)
    
;named let
(define (fac1 n)
  (let loop ((i 1) (c 1))
    (if (eq? i n)
        (* i c)
        (loop (+ i 1) (* c i)))))
  
(fac1 5)