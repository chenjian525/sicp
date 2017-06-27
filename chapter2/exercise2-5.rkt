#lang racket
(define (pow x y)
  (define (iter_pow x y r)
    (if (= y 0)
        r
        (iter_pow x (- y 1) (* r x))))
  (iter_pow x y 1))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (get_counts num base)
  (define (iter_get_counts num base count)
    (if (= 0 (remainder num (pow base count)))
        (iter_get_counts num base (+ count 1))
        (- count 1)))
  (iter_get_counts num base 1))

(define (cdr z)
  (get_counts z 3))

(define (car z)
  (get_counts z 2))