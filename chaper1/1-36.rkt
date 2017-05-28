#lang racket
(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2))

(define (fix-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))

  (define (try guess count)
    (let ((next (f guess)))
      (display "The ")
      (display count)
      (display " times guess: ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))

#(fix-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)

#(fix-point (lambda (x) (/ (log 1000) (log x))) 2)