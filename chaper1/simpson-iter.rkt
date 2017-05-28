#lang racket


(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (simpson f a b n)
  
  (define h (/ (- b a) n))
  
  (define (y k)
    (f (+ a (* k h))))
  
  (define (factor k)
    (cond ((or (= k 0) (= k n))
           1)
          ((odd? k)
           4)
          (else
           2)))
  (define (next k) (+ k 1))
  (define (term k) (* (factor k) (y k)))
  (if (not (even? n)) (error "n can not be an odd!")
      (* (/ h 3) (sum term (exact->inexact 0) next n))))