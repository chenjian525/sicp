#lang racket
(define (make-interval a b) (cons a b))

(define (lower-bound interval) (min (car interval) (cdr interval)))

(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (upper-bound y)))
        (p2 (- (upper-bound x) (lower-bound y))))
    (make-interval p1 p2)))

(define (display-interval interval)
  (newline)
  (display "[")
  (display (lower-bound interval))
  (display " , ")
  (display (upper-bound interval))
  (display "]"))
