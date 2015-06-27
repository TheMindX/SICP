#lang racket
(require "common.rkt")
(require "chapter1.rkt")
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


;bound calc
(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (define l (+ (lower-bound x) (lower-bound y)))
  (define u (+ (upper-bound x) (upper-bound y)))
  (make-interval l u))

(define (mul-iterval x y)
  (define l lower-bound)
  (define u upper-bound)
  (define p1 (* (l x) (l y)))
  (define p2 (* (l x) (u y)))
  (define p3 (* (u x) (l y)))
  (define p4 (* (u x) (u y)))
  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))



(define (sub-interval x y)
  (define l lower-bound)
  (define u upper-bound)
  (define m make-interval)
  (m (- (l x) (u y)) (- (u x) (l y))))


(define (div-interval x y)
  (define l lower-bound)
  (define u upper-bound)
  (define m make-interval)
  (define (test-interval x)
    (if (<= (* (l x) (u x)) 0)
        (error (format "div-interval ~s ~s " x y) )
        x))
  
  (define x1 (test-interval x))
  (define y1 (test-interval y))
  (define p1 (/ (l x) (l y)))
  (define p2 (/ (l x) (u y)))
  (define p3 (/ (u x) (l y)))
  (define p4 (/ (u x) (u y)))
  (m (min p1 p2 p3 p4) (max p1 p2 p3 p4)))


; todo 2.16


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))



(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(define (append la lb)
  (if (null? la)
      lb
      (cons (car la) (append (cdr la) lb))))
;(append (list 3 4 5 6) (list 8 9))


(define (lastof x)
  (define v1 (car x))
  (define v2 (cdr x))
  (if (null? v2)
      v1
      (lastof v2)))

;(lastof (list 3 5 8 11))

(define (reverse_s ls)
  (if (null? ls)
      null
      (append (reverse_s (cdr ls)) (cons (car ls) null))))

;(reverse_s '(1 5 8 7 2))


;2-19
(define (cc amount coins)
  (if (null? coins)
      0
      (let ((cur (car coins)))
        (cond 
          ((= amount cur) (+ 1 (cc amount (cdr coins))))
          ((< amount cur) 0)
          (else 
           (+ 
            (cc amount (cdr coins))
            (cc (- amount cur) coins)))))))


;(cc 100 (list 50 25))
(define (filter t y)
  (define (f) (car y))
  (define (o) (cdr y))
  (if (null? y) null
      (if (t (f))
          (cons (f) (filter t (o)))
          (filter t (o)))))

(define (same_parity . x)
  
  (define f (car x))
  (define l (cdr x))
  (if (null? x)
      null
      (let ((t (if (even? f)
                   even?
                   (lambda (x)
                     (not (even? x))))))
        (cons f (filter t l)))))

;(same_parity 3 4 5 6)

(define (map_mul f . ls)
  (define (m f ls)
    (define (x) (car ls))
    (define (xs) (cdr ls))
    (if (null? ls)
        null
        (cons (f (x)) (m f (xs)))))
  (m f ls))

;(map (lambda (x) (* x x)) 2 3 4)


;this is fast accum
(define (fold_l f a ls)
  (cond
    ((null? ls) a)
    (else
     (let* ((x (car ls))
            (xs (cdr ls))
            (next (f x a)))
       (fold_l f next xs)))))


(define (fold_r f a ls)
  (cond
    ((null? ls) a)
    (else
     (let* ((x (car ls))
            (xs (cdr ls)))
       
       (f x (fold_r f a xs))))))


(define (append1 a1 a2)
  (fold_r 
   cons
   a2
   a1))

;(append1 '(1 2) '(3 4))

(define (map f ls)
  (fold_r
   (lambda (i a)
     (cons (f i) a))
   null
   ls))

;(map (lambda (i) (* i i))   (list 1 2 3 4))

(define (length1 ls)
  (fold_l
   (lambda (i a) (+ a 1))
   0
   ls))

;(length1 '(1 2 3 4 6))


(define (accumulate-n op init seqs)
  (define (fs) (map car seqs))
  (define (os)
    (let* (
           (pre_os (map cdr seqs))
           (fir_os (car pre_os)))
      (if (null? fir_os)
          null
          pre_os)))
  
  
  (if (null? seqs) 
      null
      (cons (fold_l op init (fs)) (accumulate-n op init (os)))))

;(accumulate-n + 0 (list (list 1 2 3) (list 1 4 3) (list 1 2 5)))

(define (enumerate-interval n m)
  (cond ((> n m) (error "n is bigger than m"))
        ((eq? n m) (list m))
        (else (cons n (enumerate-interval (+ n 1) m)))))

(define (flatmap proc seq)
  (fold_l append null (map proc seq)))

(define (prime_pair n)
  
  (define (pairs_under n)
    (map (lambda (i) (list i n)) (enumerate-interval 1 (- n 1))))
  (define (pairs_in n)
    (flatmap pairs_under (enumerate-interval 2 n)))
  (filter 
   (lambda (p) (ferma-test (+ (car p) (car (cdr p)))))
   (pairs_in n)))

;(prime_pair 5)







;八皇后
;chess layout
(define (eight_queen)
  ;已有的layout新位置是否安全
  (define (safe? layout n)
    (define layout_1
      (fold_r (lambda (i a) (cons (list i (+ 1 (length a))) a)) null layout))
    (not (fold_r (lambda (i a) (or a 
                                   (eq? (car i) n)
                                   (eq? (+ (car i) (car (cdr i))) n)
                                   (eq? (- (car i) (car (cdr i))) n))) false layout_1)))
  (define (n_queen board_size)
    ;n位置时的所有可能排列
    (define (q_col n)
      (if (eq? n 0)
          '( ())
          (letrec 
              ((layouts (q_col (- n 1)))
               ;基本n位置的n+1位置的组合
               (safe_poss 
                (lambda (layout)
                  (filter (lambda (n) (safe? layout n)) (enumerate-interval 1 8))))
               (newlayouts (flatmap
                            (lambda (layout)
                              (map
                               (lambda (i) (append layout (list i)))
                               (safe_poss layout)))
                            layouts)))
            newlayouts)))
    (q_col board_size) )
  (n_queen 8))


;(eight_queen)

;(newlayouts '( (6 3 1 7 5 8 2)))


;代数系统,符号求导
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? e)
  (eq? (car e) '+))

(define (addend e)
  (car (cdr e)))

(define (augend e)
  (car (cdr (cdr e))))

(define (make-sum a1 a2)
  (if (eq? a1 0)
    a2
    (if (eq? a2 0)
      a1
      (list '+ a1 a2))))

(define (product? e)
  (eq? (car e) '*))

(define (multiplier e)
  (car (cdr e)))

(define (multiplicand e)
  (car (cdr (cdr e))))

(define (make-product a1 a2)
  (if (eq? a1 1)
    a2
    (if (eq? a2 1)
      a1
      (list '* a1 a2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknown expression type"))))

;(deriv '(+ x 3) 'x)
            
;无序表转为有序树
(define (stree-insert elem tree)
  (if (null? tree)
      (list elem null null)
      (let ((entry (car tree))
            (lt (car (cdr tree)))
            (rt (car (cdr (cdr tree)))))
        (if (< elem entry)
            (list entry (stree-insert elem lt) rt)
            (list entry lt (stree-insert elem rt))))))
                  
(define (ustable2stree t)
  (fold_l stree-insert '() t))
  
  
(define (stree2stable tree)
  (if (null? tree)
    null
    (let ((entry (car tree))
        (lt (car (cdr tree)))
        (rt (car (cdr (cdr tree)))))
      (append (append (stree2stable lt) (list entry))
              (stree2stable rt)))))
              


(define (stable2bt st)
  (if (null? st)
    null
    (let* ((len (length st))
          (midl (quotient (- len 1) 2) )
          (ltable (take st midl))
          (restTable (drop st midl))
          (entry (car restTable))
          (rtable (cdr restTable)))
         (list entry (stable2bt ltable) (stable2bt rtable)))
  ))


(define (ustable2bTree t)
  (stable2bt (stree2stable (ustable2stree t))))

;(ustable2bTree '(5 3 7 2 1 6 3))




;huffman树的表示
;left 结构
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))


(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

;tree node结构
(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
  
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (next-subtree bit subtree)
  (if (eq? bit 0)
    (left-branch subtree)
    (right-branch subtree)))

;huffman 解码
(define (decode bits tree)
  (define (decode1 bits subtree)
    (if 
      (eq? bits null)
        (if 
          (eq? subtree tree)
          null
          (error "no code complete"))
      (if (leaf? subtree)
        (cons
          (symbol-leaf subtree)
          (decode1 bits tree))
        (decode1
          (cdr bits)
          (next-subtree (car bits) subtree)))))
  (decode1 (common.log bits) (common.log tree)))



(define (adjoin-set x set)
  (cond 
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))




(define (make-leaf-set pairs)
  (if (null? pairs)
    null
    (let*
      ((pair (car pairs))
       (leafItem (make-leaf (car pair) (cadr pair))))
      (adjoin-set leafItem (make-leaf-set (cdr pairs))))))


;(make-leaf-set '((a 1) (b 3) (c 2)))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)


(define (myAdd a b)
  (+ (common.log a) b))

;(myAdd 3 4)


