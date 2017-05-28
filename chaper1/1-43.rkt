#lang racket
(define (compose f)
  (lambda (x) (f (f x))))

(define square (lambda (x) (* x x)))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f) (- n 1))))
      