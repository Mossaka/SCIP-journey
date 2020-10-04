#lang racket

;; Hierarchical Data and the Closure Property

(cons 1 (cons 2 (cons 3 (cons 4 null))))
(list 1 2 3 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;; exercise 2.17

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

;; exercise 2.18

(define (reverse items)
  (define (helper items ans)
    (if (null? items)
        ans
        (helper (cdr items) (cons (car items) ans))))
  (helper items '()))

;; exercise 2.20

; (define f (lambda (x y . z) ⟨body⟩))
; (define g (lambda w ⟨body⟩))

(define (same-parity a . rest)
    (cons a (filter (λ (b) (= (modulo a 2) (modulo b 2))) rest)))


;; mapping over the list



;; exercise 2.21

(define (square-list items)
  (map (λ (x) (* x x)) items))

;; exercise 2.23

(define (for-each f items)
  (map f items)
  true)

;; exercise 2.27

(define (deep-reverse items)
  (let ([ritems (reverse items)])
       (if (null? ritems)
           ritems
           (cons (reverse (car ritems)) 
                 (deep-reverse (cdr ritems))))))

;; exercise 2.28

(define (fringe tree)
  (if (null? tree)
      tree
      (let ([x (car tree)])
           (if (not (pair? x))
               (cons x (fringe (cdr tree)))
               (append (fringe x) (fringe (cdr tree)))))))

(define fringe2 flatten)

;; exercise 2.30
(require point-free)
(define (square-tree2 tree) 
        (map (λ (x) (if (pair? x) 
                        (square-tree x)
                        (* x x))) tree))

;; exercise 2.31

(define (square x) (* x x))
(define (tree-map proc tree)
  (map (λ (subtree) (if (pair? subtree)
                        (tree-map proc subtree)
                        (proc subtree))) tree))
(define (square-tree tree) (tree-map square tree))

;; exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (λ (x y) (+ y 1)) 0 sequence))

;; exercise 2.34

(define (horner-eval x cofficient-sequence)
  (accumulate (λ (this-coeff higher-terms)
                 (+ this-coeff (* x higher-terms)))
              0
              cofficient-sequence))

;; exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (r) (dot-product r v)) m))

(define (tranpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (map (λ (v) (matrix-*-vector (tranpose n) v)) m))

;; exercise 2.38

;; Nested Mappings

(define (flatmap proc seq)
(accumulate append null (map proc seq)))

;; exercise 2.40

(define (add1 x) (+ x 1))

(define (unique-pairs n)
  (flatmap (λ (x) 
              (map (λ (y) (list x y)) 
                   (range 1 x))) 
           (range 2 (add1 n))))

;; exercise 2.41

(define (triples n s)
  (let ([pairs (unique-pairs n)])
       (filter-map (λ (pair) 
                      (let ([res (- s (accumulate + 0 pair))]) 
                           (and (< res (add1 n)) (and (not (member res pair)) (and (positive? res) (cons res pair)))))) 
                   pairs)))

;; exercise 2.44



;; exercise 2.45