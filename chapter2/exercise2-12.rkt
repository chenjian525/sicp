#lang racket
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (make-interval a b) (cons a b))

(define lower-bound car)

(define upper-bound cdr)

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (percent i)
  (let ((width (/ (- (upper-bound i) (lower-bound i)) 2)))
    (/ width (center i))))