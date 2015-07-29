#lang racket

(require "common.rkt")

(force (delay (common.println 3)) )

(define lazy-test (lazy (common.println "promise calc") (lazy (+ 100 1000))))

(force lazy-test)
(force lazy-test)


(define-syntax-rule (mydelay exp)
  (lambda() exp))

(define-syntax-rule (myforce exp)
  (exp))


(define-syntax mydelay1
  (syntax-rules ()
    [(_ exp)
     (lambda () exp)]))


(mydelay1 (begin (common.println "asdf") 3))

(define-syntax infix
  (syntax-rules (plus sub)
    [(_ a plus b)
     (+ a b)]
     [(_ a sub b)
     (- a b)])
  )

(infix 3 plus 4)
(infix 3 sub 4)