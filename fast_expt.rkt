#lang racket

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


(fast_expt 2 31)