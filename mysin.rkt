#lang racket

(define (mysin a)
  (define (cube x) (* x x x))
  (if (< a 0.001)
      a
      (let ((sindiv3 (mysin (/ a 3))))
        (- (* 3 sindiv3) (* 4 (cube sindiv3))))))
      

(exact->inexact (mysin 5) )


