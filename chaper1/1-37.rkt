#lang racket
(define (cont-frac n d k)
  (define (cf i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

(define (cont-frac-iter n d k)
  (define (cf i result)
    (if (= i 1)
        result
        (cf (- i 1) (/ (n i) (+ (d i) result)))))
  (cf (- k 1) (/ (n k) (d k))))

(define (gold-ratio k)
  (+ 1 (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) k)))