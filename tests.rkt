#lang racket

(require "23-tree.rkt" rackunit)
(require/expose "23-tree.rkt" (pair singleton balanced?))

(define (insert-range a b step [t 'emp])
  (for/fold ([t t])
    ([v (in-range a b step)])
    (23-tree-insert t v)))

(define (delete-range a b step t)
  (for/fold ([t t])
    ([v (in-range a b step)])
    (23-tree-delete t v)))

(define (test-balanced t)
  (define-values (b? h) (balanced? t))
  (define s (23-tree-size t))
  (check-true b?)
  (check-true (or (= 0 s) (= 1 s)
                  (and (<= (floor (logn 3 s)) h)
                       (<= h (ceiling (logn 2 s)))))))

(define (logn b n)
  (/ (log n) (log b)))

(test-begin 
 (check-equal? (23-tree-find (singleton 255) 255) 255)
 (check-equal? (23-tree-find (23-tree-insert 'emp 255) 255) 255)
 (check-equal? (23-tree-size (23-tree-insert (pair 25 50) 50)) 2))

(test-case 
 "Balanced"
 (test-balanced 'emp)
 (test-balanced (singleton 10))
 (test-balanced (pair 10 20))
 (test-balanced (23-tree-insert (pair 10 20) 30)))

(test-begin
 (define t (insert-range 0 100 1))
 (check-equal? (23-tree-size t) 100)   
 (for ([v (in-range 100)])
   (check-equal? (23-tree-find t v) v)) 
 (for ([v (in-range 0.5 100 1)])
   (check-false (23-tree-find t v)))
 (test-balanced t))

(test-case
 "Insert backwards / forwards"
 (define tb (insert-range 100 0 -1))
 (define tf (insert-range 50 2001 1 tb))
 (check-equal? (23-tree-size tf) 2000))

(test-case 
 "Simple deletions"
 (check-equal? (23-tree-delete 'emp 50) 'emp)
 (check-equal? (23-tree-delete (singleton 10) 10) 'emp)
 (check-equal? (23-tree-find (23-tree-delete (list->23-tree '(10 20 30 40)) 50) 10) 10))

(test-case 
 "Deletion"
 (define t (insert-range 0 1001 1))
 (define td (delete-range 1001 49 -1 t))
 (check-equal? (23-tree-size td) 50)
 (test-balanced td)   
 (for ([v (in-range 50)])
   (check-equal? (23-tree-find td v) v))
 (for ([v (in-range 50 1001)])
   (check-false (23-tree-find td v) v))
 (check-equal? (delete-range 0 50 1 td) 'emp))

(test-case 
 "Delete non-existent"
 (define t (insert-range 0 1001 1))
 (check-equal? (23-tree-size (delete-range 1001 2000 1 t)) 1001))

(test-case 
 "Shuffled insert/delete"
 (define num 2500) 
 (define lst (shuffle (build-list num values)))
 (define t0 (list->23-tree lst))
 (check-equal? (delete-range 0 num 1 t0) 'emp)
 (check-equal? (for/fold ([t t0])
                 ([v (in-list lst)])
                 (23-tree-delete t v))
               'emp)
 (define t1 (insert-range 0 num 1 (delete-range 0 (- num 500) 1 t0)))
 (check-equal? (23-tree-size t1) num))
