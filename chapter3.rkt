#lang racket
(require "common.rkt")
(require "chapter1.rkt")

(require 2htdp/image)
(require compatibility/mlist)

#|
;one way to impl mycons
(define (mycons a b)
  (define (dispatch isCar)
    (if isCar
        a
        b))
  dispatch)

(define (mycar c)
  (c #t))

(define (mycdr c)
  (c #f))

(define testmycons
  (let ((t (mycons 3 4)))
    (begin (common.log (mycar t))
           (common.log (mycdr t)))))
|#


;anther way to impl mycons, mutable list
(define (mycons a b)
  (lambda (cmd)
    (cond ((eq? cmd 'car) a)
          ((eq? cmd 'cdr) b)
          ((eq? cmd 'set-car) 
           (lambda (x) (set! a x)))
          ((eq? cmd 'set-cdr) 
           (lambda (x) (set! b x))))))
  
(define (mycar c)
  (c 'car))

(define (mycdr c)
  (c 'cdr))
  
(define (myset-car c x)
  ((c 'set-car) x))
  
(define (myset-cdr c x)
  ((c 'set-cdr) x))


(define (test_my_cons)
  (let ((t (mycons 3 4)))
    (begin (common.log (mycar t))
           (common.log (mycdr t))
           (myset-car t 30)
           (common.log (mycar t))
           (myset-cdr t 40)
           (common.log (mycdr t)))))


; impl queue data
(define (make-queue)
  (mycons null null))

(define (front-queue q)
  (mycar (mycar q)))


(define (null-queue? q)
  (null? (mycar q)))

(define (push-queue q item)
  (let ((newpair (mycons item null)))
    (if (null? (mycar q))
        (begin (myset-car q newpair)
               (myset-cdr q newpair))
        (begin (myset-cdr (mycdr q) newpair)
               (myset-cdr q newpair)))))


(define (pop-queue q)
  (if (eq? (mycar q) (mycdr q))
      (begin (myset-car q null)
             (myset-cdr q null))
      (myset-car q (mycdr (mycar q)))))



(define (test-myqueue)
  (letrec ((q (make-queue)))
            
           (begin 
             (push-queue q 123)
             (push-queue q 456)
             (common.log (front-queue q))
             (pop-queue q)
             (common.log (front-queue q))
             (pop-queue q)
             (common.log (null-queue? q)))))
            
  
  
  
               
        



