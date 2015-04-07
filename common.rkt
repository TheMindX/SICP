#lang racket

(provide common.square)
(provide common.cube)
(provide common.mod)

(define (common.square n)
  (* n n))

(define (common.cube n)
  (* n n n))

(define (common.mod v d)
  (if (< v d) v (common.mod (- v d) d)))
  

