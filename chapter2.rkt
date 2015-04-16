#lang racket
(require "common.rkt")

(define (make-rat n d)
  (define cd (common.gcd n d))
  (cons (/ n cd) (/ d cd)))

(define (rat-num r)
  (car r))

(define (rat-denom r)
  (cdr r))


(gcd 22 11)


(define (add-rat x y)
  (let* ((n1 (rat-num x))
         (n2 (rat-num y))
         (d1 (rat-denom x))
         (d2 (rat-denom y))
         (n1d2 (* n1 d2))
         (n2d1 (* n2 d1))
         (d1d2 (* d1 d2)))
    (make-rat (+ n1d2 n2d1) d1d2)))


(define (sub-rat x y)
  (let* ((n1 (rat-num x))
         (n2 (rat-num y))
         (d1 (rat-denom x))
         (d2 (rat-denom y))
         (n1d2 (* n1 d2))
         (n2d1 (* n2 d1))
         (d1d2 (* d1 d2)))
    (make-rat (- n1d2 n2d1) d1d2)))

(define (mul-rat x y)
  (let* ((n1 (rat-num x))
         (n2 (rat-num y))
         (d1 (rat-denom x))
         (d2 (rat-denom y)))
    (make-rat (* n1 n2) (* d1 d2))))


(define (reverse x)
  (make-rat (rat-denom x) (rat-num x)))


(define (div-rat x y)
  (mul-rat x (reverse y)))








