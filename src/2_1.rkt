#lang racket

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; exercise 2.1
(define (make-rat n d)
  (cond ([and (negative? n) (negative? d)] (make-rat (- n) (- d)))
        ([negative? d] (make-rat (- n) (- d)))
        (else (let ((g (gcd n d)))
                   (cons (/ n g) (/ d g))))))
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; exercise 2.2
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (average x y) 
  (/ (+ x y) 2.0))

(define (midpoint-segment line)
  (let ([start (start-segment line)] 
        [end (end-segment line)])
       (make-point (average (x-point start) (x-point end)) 
                   (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; exercise 2.3

(define make-rectangle cons)
(define (perimeter rec) 
  (let ([a (car rec)]
        [b (cdr rec)])
       (+ (* 2 (- (y-point a) 
                  (y-point b))) 
          (* 2 (- (x-point b) 
                  (x-point a))))))

; exercise 2.4
(define (cdr2 z)
  (z (λ (p q) q)))

(define one (λ f (lambda (x) (f x))))
(define two (λ f (lambda f (lambda (x) (f (f x))))))

(define (add a b)
  (λ f (λ x (a f (b f x)))))