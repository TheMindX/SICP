#lang racket
(require "common.rkt")
#|
(define (make-rat n d)
  (define cd (common.gcd n d))
  (cons (/ n cd) (/ d cd)))

(define (rat-num r)
  (car r))

(define (rat-denom r)
  (cdr r)) 


;other way to cons data
(define (make-rat x y)
  (define gcd (common.gcd x y))
  (lambda (op)
    (if op
        (/ x gcd)
        (/ y gcd))))

(define (rat-num r)
  (r #t))

(define (rat-denom r)
  (r #f))
|#


;and other way

(define (make-rat x y)
  (define comdiv (common.gcd x y))
  (* (expt 2 (/ x comdiv) ) (expt 3 (/ y comdiv))))

(define (rat-num r)
  (define (find2 n i)
    (if (even? n)
        (find2 (/ n 2) (+ i 1))
        i))
  (find2 r 0))

(define (rat-denom r)
  (define (find3 n i)
    (define (div3? n) (eq? 0 (modulo n 3)))
    (if (div3? n)
        (find3 (/ n 3) (+ i 1))
        i))
  (find3 r 0))

(define (tostring r)
  (list (rat-num r) (rat-denom r)))


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


; (tostring (add-rat (make-rat 10 20) (make-rat 3 4)))



;church-num
(define zero 
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x) (f x))))


(define two
  (lambda (f)
          (lambda (x) (f(f x)))))

(define (add x y)
  (lambda (f)
          ((f x) y)))

          
;           
           
           