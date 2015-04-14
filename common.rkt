#lang racket

(provide common.square) ;平方
(provide common.cube) ;立方
(provide common.mod) ;取模
(provide common.even?) ;是否偶数
(provide common.println) ;打印
(provide common.constant) ;常数

(define (common.square n)
  (* n n))

(define (common.cube n)
  (* n n n))

(define (common.mod v d)
  (if (< v d) v (common.mod (- v d) d)))


(define (common.even? x)
  (eq? (common.mod x 2) 0))
  

(define (common.println x)
  (display x)
  (display "\n"))


(define (common.constant n)
  (lambda (x) n))
  