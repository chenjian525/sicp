#lang racket
;zero是个接收一个参数f的过程,它执行后会返回另一个接收一个参数x的过程,这个过程执行后会返回参数x本身。
;one是个接收一个参数f的过程,它执行后会返回另一个接收一个参数x的过程,这个过程执行后会返回f调用x的结果。
;所以((zero f) x) 等于 x
;((one f) x) 等于 (f x)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

