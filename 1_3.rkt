#lang racket

; procedures as arguments
; summation
(define (sum term a next b)
    (if (> a b)
       0
       (+ (term a)
          (sum term (next a) next b))))


(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))

(define (cube x) (* x x x))

; execise 1.29

(define (even? n) 
    (= (remainder n 2) 1))

(define (simpson f a b n) 
    (define h (/ (- b a) n))
    (define (helper f a b n step) 
        (define (add-h a) 
            (+ a h))
        (define (coff-y a)
            (cond [(or (= a b) (= a 0)) 1.0]
                [(even? a) 2.0]
                [else 4.0]))
        (if (> step n)
             0
             (+ (* (* (/ h 3.0) (coff-y step)) (f a))
                (helper f (add-h a) b n (+ step 1)))))
    (helper f a b n 0))

; exercise 1.30

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (integral-iter f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum-iter f (+ a (/ dx 2.0)) add-dx b)
       dx))

; exercise 1.31

(define (product term a next b)
    (if (> a b)
       1
       (* (term a)
          (product term (next a) next b))))

(define (identity n) n)
(define (add1 n) (+ n 1))

(define (factorial n)
    (product identity 1 add1 n))
;TODO

; exercise 1.32
;TODO

; exercise 1.33
;TODO
