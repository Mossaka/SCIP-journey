#lang racket

(require compose-app)
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
    (* (sum f 
             (+ a (/ dx 2.0)) 
             (lambda (x) (+ x dx)) 
             b)
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
    (prodcut-iter identity 1 add1 n))

(define (pi-appro n)
    (define (term n)
        (/ n (add1 n)))
    (* (* 4 (/ (if (even? n) (+ n 2) (add1 n)) 3)) 
        (* (prodcut-iter term 4.0 (add1 .. add1) (if (even? n) n (- n 1))) 
            (prodcut-iter term 2.0 (add1 .. add1) (if (even? n) n (+ n 1))))))

;"1_3.rkt"> (pi-appro 100001)
;3.141639776733336
;"1_3.rkt"> (pi-appro 100000)
;3.1416083615921333

(define (prodcut-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))

; exercise 1.32

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

(define (sum2 term a next b)
    (accumulate + 0 term a next b))
(define (product2 term a next b)
    (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            null-value
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))
; exercise 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (a > b)
        null-value
        (let ([x (term a)])
             (if (filter x)
                 (combiner x (filtered-accumulate filter combiner null-value term (next a) next b))
                 (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (f g) (g 2))

;; finding zeros
(define (average x y)
    (/ (+ x y) 2))
(define (search f neg-point pos-point)
    (let ([midpoint (average neg-point pos-point)])
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ([test-value (f midpoint)])
                (cond ((positive? test-value)
                       (search f neg-point midpoint))
                      ((negative? test-value)
                       (search f midpoint pos-point))
                      (else midpoint))))))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
               (search f a b))
              ((and (negative? b-value) (positive? a-value))
               (search f b a))
              (else
               (error "Values are not of opposite sign" a b)))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

#| 
"1_3.rkt"> (half-interval-method sin 2.0 4.0)
3.14111328125
"1_3.rkt"> (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
           1.0
           2.0)
1.89306640625 
|#


(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (display guess)
        (newline)
        (let ([next (f guess)])
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

#|
"1_3.rkt"> (fixed-point cos 1.0)
0.7390822985224024
"1_3.rkt"> (fixed-point (lambda (y) (+ (sin y) (cos y)))
           1.0)
1.2587315962971173
|#

; exercise 1.35
#|
"1_3.rkt"> (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
1.6180327868852458
|#

; exercise 1.36
#|
"1_3.rkt"> (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
2.0
9.965784284662087
3.004472209841214
6.279195757507157
3.759850702401539
5.215843784925895
4.182207192401397
4.8277650983445906
4.387593384662677
4.671250085763899
4.481403616895052
4.6053657460929
4.5230849678718865
4.577114682047341
4.541382480151454
4.564903245230833
4.549372679303342
4.559606491913287
4.552853875788271
4.557305529748263
4.554369064436181
4.556305311532999
4.555028263573554
4.555870396702851
4.555315001192079
4.5556812635433275
4.555439715736846
4.555599009998291
4.555493957531389
4.555563237292884
4.555517548417651
4.555547679306398
4.555527808516254
4.555540912917957
4.555532270803653
|#

; exercise 1.37
(define (cont-frac n d k)
    (define (iter k result)
        ;(newline)
        ;(display result)
        (if (< k 0)
            result
            (iter (- k 1) (/ (n k) (+ (d k) result)))))
    (iter k 0))

; exercise 1.38
(define (d i)
    (if (= i 0)
        1.0
        (let ([j (- i 1)])
            (if (= (modulo j 3) 0)
                (+ 2 (* 2 (quotient j 3)))
                1
                ))))

(define (e k)
    (+ 2 (cont-frac (lambda (x) 1) d k)))

;; procedures as returned values

(define (average-damp f) (lambda (x) (average x (f x))))

(define dx 0.00001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define (square x) (* x x))

(define (sqrt x)
    (newtons-method
        (lambda (y) (- (square y) x)) 1.0))

; abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

; exercise 1.40
(define (cubic a b c)
    (newtons-method
        (lambda (x) (+ (+ (+ (cube x) (* a (square x))) (* b x)) c)) 1.0))

; exercise 1.41

(define (double f)
    (lambda (x) (f (f x))))

#|
"1_3.rkt"> (((double (double double)) inc) 5)
21
|#

; exercise 1.42

(define (compose f g) 
    (lambda (x) (f (g x))))

; exercise 1.43
(define (repeated f n)
    (if (< n 2)
        f
        (repeated (compose f f) (- n 1))))

; exercise 1.44

(define (smooth f)
    (lambda (x) (average (average (f (- x dx)) (f x)) (f (+ x dx)))))

(define (n-fold-smoothed f n)
    (repeated smooth f n))

; exercise 1.46

(define (iterative-improve f g)
    (define (iter guess)
        (if (f guess)
            guess
            (iter (g guess))))
    iter)