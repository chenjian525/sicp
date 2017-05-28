#lang racket
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (factor k) (cond ((or (= k 0) (= k n)) 1)
                           ((even? k) 2) (else 4)))
  (define (term k) (* (factor k) (y k)))
  (define (sum term a next b)
    (if (> a b) 0 (+ (term a) (sum term (next a) next b))))
  (define (next k) (+ k 1))
  (if (not (even? n)) (error "n can not be an odd!")
      (* (/ h 3) (sum term (exact->inexact 0) next n))))

(define (cube x) (* x x x))