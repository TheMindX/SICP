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
  (cons 'mycons 
        (lambda (cmd)
          (cond ((eq? cmd 'car) a)
                ((eq? cmd 'cdr) b)
                ((eq? cmd 'set-car) 
                 (lambda (x) (set! a x)))
                ((eq? cmd 'set-cdr) 
                 (lambda (x) (set! b x)))))))
  
(define (mycar c)
  ((cdr c) 'car))

(define (mycdr c)
  ((cdr c) 'cdr))
  
(define (myset-car c x)
  (((cdr c) 'set-car) x))
  
(define (myset-cdr c x)
  (((cdr c) 'set-cdr) x))


(define (test_my_cons)
  (let ((t (mycons 3 4)))
    (begin (common.log (mycar t))
           (common.log (mycdr t))
           (myset-car t 30)
           (common.log (mycar t))
           (myset-cdr t 40)
           (common.log (mycdr t)))))


; impl queue data
(define (mymake-queue)
  (mycons null null))

(define (myfront-queue q)
  (mycar (mycar q)))


(define (mynull-queue? q)
  (null? (mycar q)))

(define (mypush-queue q item)
  (let ((newpair (mycons item null)))
    (if (null? (mycar q))
        (begin (myset-car q newpair)
               (myset-cdr q newpair))
        (begin (myset-cdr (mycdr q) newpair)
               (myset-cdr q newpair)))))


(define (mypop-queue q)
  (if (eq? (mycar q) (mycdr q))
      (begin (myset-car q null)
             (myset-cdr q null))
      (myset-car q (mycdr (mycar q)))))


#|
(define (test-myqueue)
  (letrec ((q (mymake-queue)))
            
           (begin 
             (mypush-queue q 123)
             (mypush-queue q 456)
             (common.log (myfront-queue q))
             (mypop-queue q)
             (common.log (myfront-queue q))
             (mypop-queue q)
             (common.log (mynull-queue? q)))))
|#
;impl table


(define (myequal? a b)
  (if (equal? a b)
      #t
      (if (and (pair? a) (pair? b))
          (if (and (eq? (car a) 'mycons)
                   (eq? (car b) 'mycons))
              (if (and (myequal? (mycar a) (mycar b))
                       (myequal? (mycdr b) (mycdr b)))
                  #t
                  #f)
              #f)
          #f)))

(define (mymake-table)
  (mymake-queue))

(define (mynull-table? t)
  (mynull-queue? t))

(define (myfind-table t key)
  (if (mynull-table? t)
      null
      (letrec ((first-item (mycar t))
               (find (lambda (ls) ;iter to find match key in list
                    (if (null? ls)
                        null
                        (letrec ((kv (mycar ls))
                                 (k (mycar kv))
                                 (v (mycdr kv))
                                 (next (mycdr ls)))
                          (if (myequal? k key)
                              v
                              (find next)))))))
        (find first-item))))
                        
(define (myinsert-table t key value);insert or update
    (if (mynull-table? t)
      (mypush-queue t (mycons key value))
      (letrec ((first-item (mycar t))
               (update (lambda (ls) ;iter to update match key in list
                    (if (null? ls)
                        (mypush-queue t (mycons key value))
                        (letrec ((kv (mycar ls))
                                 (k (mycar kv))
                                 (v (mycdr kv))
                                 (next (mycdr ls)))
                          (if (myequal? k key)
                              (myset-cdr kv value)
                              (update next)))))))
        (update first-item))))



;(define test-my-table
;  (letrec ((t (mymake-table)))
;    (begin (myinsert-table t '(2 5) 3)
;           (myinsert-table t '(2 5) 5)
;           (myinsert-table t 1 4)
;           (common.log (myfind-table t '(2 5) )))))

           
        
;3.27 memoization
(define (fib n)
  (if (eq? n 1)
      1
      (if (eq? n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define global-table (mymake-table))

;(define (memofib n)
;  (cond ((eq? n 1) 1)
;        ((eq? n 2) 1)
;        (else 
;         (let ((val (myfind-table global-table '(fib n))))
;           (if (null? val)
;               (let ((forceval (+ (memofib (- n 1)) (memofib (- n 2)))))
;                 (begin (myinsert-table global-table n forceval)
;                        forceval))
;               val)))))


(define (memo func)
  (lambda (key)
    (let ((val (myfind-table global-table key)))
      (if (null? val)
        (let ((forceval (func key)))
          (begin (myinsert-table global-table key forceval)
            forceval))
        val))))


(define (memofib n)
  (let ((mfib (memo fib)))
    (cond 
      ((eq? n 1) 1)
      ((eq? n 2) 1)
      (else (+ (memofib (- n 1)) (memofib (- n 2)))))))

;((memo fib) 28)
;(memofib 38)

;3.24

  
;stream
(define-syntax-rule (mdelay exp)
    (let ((v null))
      (lambda ()
        (if (null? v)
            (begin (set! v exp) v)
            v))))


(define-syntax-rule (mforce lv)
   (lv))

(define-syntax-rule (cons-stream a b)
  (cons a (mdelay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (mforce (cdr stream)))

(define (make-mstream init fnext)
  (cons-stream init (make-mstream (fnext init) fnext)))


(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define integers (integers-from 1))


(define (stream-filter stream pred)
  (let find ((test-stream stream))
    (let ((first (stream-car test-stream)))
      (if (pred first)
        (cons-stream first 
                     (stream-filter (stream-cdr test-stream) pred))
        (find (stream-cdr test-stream))))))
  


(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr integers)))))

(define (print_stream stream n)
  (let loop ((i  n) 
             (first (stream-car stream) )
             (rest (stream-cdr stream)))
    (if (eq? i 0)
        null
        (if (eq? rest null)
            null
            (begin (common.println first)
                   (loop (- i 1) (stream-car rest) (stream-cdr rest)))))))
        
             
  
  
;now let we find primary

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter stream
                         (lambda (n) (not (eq? 0 (modulo n (stream-car stream)))))))))
   
   
