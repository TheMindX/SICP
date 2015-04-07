#lang racket

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

(define (sq x)
  (sq_iter (/ x 2) x))


(exact->inexact (sq 2))