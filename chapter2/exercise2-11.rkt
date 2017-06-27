#lang racket
;; (cons a b)是一个过程，它接收一个过程m作为参数，返回(m a b)
;; let后面的所有赋值要用()包起来，let和赋值之后所做的操作要一起包起来: (let (assign) operation)
;; cond后面每种情况自己用()包起来即可，最后统一包起来: (cond (condition1)(condition2)(else))
;; if类似三元表达式: (if (condition) operation1 operation2) 满足condition时执行operation1,否则执行operation2
(define (cons x y)
  (lambda (m) (m x y)))

(define (car c)
  (c (lambda (p q) p)))

(define (cdr c)
  (c (lambda (p q) q)))

(define (make-interval x y) (cons x y))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (print-interval name i)
  (newline)
  (display name)
  (display ": [")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define (mul-interval x y)
  
  (define (endpoint-sign x)
    (cond ((and (>= (upper-bound x) 0)
                (>= (lower-bound x) 0))
           1)
          ((and (< (upper-bound x) 0)
                (< (lower-bound x) 0))
           -1)
          (else 0)))

  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-up (upper-bound x))
        (x-lo (lower-bound x))
        (y-up (upper-bound y))
        (y-lo (lower-bound y)))

    (cond ((> es-x 0)
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-lo) (* x-up y-up)))
                 ((< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-up)))
                 (else
                  (make-interval (* x-up y-lo) (* x-up y-up)))))
          ((< es-x 0)
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-up) (* x-up y-lo)))
                 ((< es-y 0)
                  (make-interval (* x-up y-up) (* x-lo y-lo)))
                 (else
                  (make-interval (* x-lo y-up) (* x-lo y-lo)))))
          (else
           (cond ((> es-y 0)
                  (make-interval (* x-lo y-up) (* x-up y-up)))
                 ((< es-y 0)
                  (make-interval (* x-up y-lo) (* x-lo y-lo)))
                 (else
                  (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                 (max (* x-up y-up) (* x-lo y-lo)))))))))
    

