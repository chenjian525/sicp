#lang racket
(define (filter-accumulate filter combiner null-value term a next b)
  (define (iter filter a result)
    (cond ((> a b) result)
          ((filter a b) (iter filter (next a) (combiner result (term a))))
          (else (iter filter (next a) result))))
  (iter filter a null-value))

(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (prime-to-each-other a b)
  (= 1 (gcd a b)))

(define (prime? n b)
  (if (= n 1) false
      (miller-rabin-test n)))

(define (miller-rabin-test n)
  (define times (ceiling (/ n 2)))
  (define (square x) (* x x))
  (define (expmode base exp m)
    (cond ((= exp 0) 1)
          ((and (not (= base 1))
                (not (= base (- m 1)))
                (= 1 (remainder (square base) m))) 0)
          ((even? exp) (remainder (square (expmode base (/ exp 2) m)) m))
          (else (remainder (* base (expmode base (- exp 1) m)) m))))
  (define (non-zero-random n)
    (define num (random n))
    (if (not (= num 0)) num (non-zero-random n)))
  (define (iter-test n times)
    (cond ((= times 0) true)
          ((= (expmode (non-zero-random n) (- n 1) n) 1)
           (iter-test n (- times 1)))
          (else false)))
  (iter-test n times))

#(filter-accumulate prime? + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b)

#(filter-accumulate prime-to-each-other * 1 (lambda (x) x) 2 (lambda (x) (+ x 1)) b) 