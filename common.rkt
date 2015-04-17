#lang racket

(provide common.square) ;平方
(provide common.cube) ;立方
(provide common.mod) ;取模
(provide common.even?) ;是否偶数
(provide common.println) ;打印
(provide common.constant) ;常数
(provide common.gcd) ;公约数
(provide common.typeof) ;类型


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



;gcd
(define (common.gcd m n)
  (cond 
    ((< m n) (common.gcd n m))
    ((eq? n 0) m)
    (else (gcd n (modulo m n)))))

;(gcd 16 8)

(define common.typeof
  (lambda (x)
    (cond ((number? x) "Number")
          ((pair? x) "Pair")
          ((string? x) "String")
          ;((port? x) "Port")
          ((vector? x) "Vector")
          ((boolean? x) "Boolean")
          ((symbol? x) "Symbol")          
		  ((procedure? x) "Procedure")          
		  ))) 

