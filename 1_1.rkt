#lang racket

(provide square)

(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(define (square x) (* x x))
(define (cube x) (* (square x) x))
(define (sum-of-squares x y)
    (+ (square x) (square y)))

; Conditional expression and predicates
(define (abs x)
    (cond ((< x 0) (- x))
    (else x)))

(define (>= x y) (or (> x y) (= x y)))

; testing applicative-order eval or normal-order eval
(define (p) (p))
(define (test x y)
    (if (= x 0) 0 y))
(test 0 (p))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))
