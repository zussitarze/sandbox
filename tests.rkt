#lang racket

(require "2-3Tree.rkt" rackunit)
(require/expose "2-3Tree.rkt" (pair singleton balanced?))

(define (insert-range a b step [t 'emp])
  (for/fold ([t t])
    ([v (in-range a b step)])
    (insert t v)))

(define (delete-range a b step t)
  (for/fold ([t t])
    ([v (in-range a b step)])
    (delete t v)))

(define (test-balanced t)
  (define-values (b? h) (balanced? t))
  (define s (size t))
  (check-true b?)
  (check-true (or (= 0 s) (= 1 s)
                  (and (<= (floor (logn 3 s)) h)
                       (<= h (ceiling (logn 2 s)))))))

(define (logn b n)
  (/ (log n) (log b)))

(test-begin 
 (check-equal? (find (singleton 255) 255) 255)
 (check-equal? (find (insert 'emp 255) 255) 255)
 (check-equal? (size (insert (pair 25 50) 50)) 2))

(test-case 
 "Balanced"
 (test-balanced 'emp)
 (test-balanced (singleton 10))
 (test-balanced (pair 10 20))
 (test-balanced (insert (pair 10 20) 30)))

(test-begin
 (define t (insert-range 0 100 1))
 (check-equal? (size t) 100)   
 (for ([v (in-range 100)])
   (check-equal? (find t v) v)) 
 (for ([v (in-range 0.5 100 1)])
   (check-false (find t v)))
 (test-balanced t))

(test-case
 "Insert backwards / forwards"
 (define tb (insert-range 100 0 -1))
 (define tf (insert-range 50 2001 1 tb))
 (check-equal? (size tf) 2000))

(test-case 
 "Simple deletions"
 (check-equal? (delete 'emp 50) 'emp)
 (check-equal? (delete (singleton 10) 10) 'emp)
 (check-equal? (find (delete (list->tree '(10 20 30 40)) 50) 10) 10))

(test-case 
 "Deletion"
 (define t (insert-range 0 1001 1))
 (define td (delete-range 1001 49 -1 t))
 (check-equal? (size td) 50)
 (test-balanced td)   
 (for ([v (in-range 50)])
   (check-equal? (find td v) v))
 (for ([v (in-range 50 1001)])
   (check-false (find td v) v))
 (check-equal? (delete-range 0 50 1 td) 'emp))

(test-case 
 "Delete non-existent"
 (define t (insert-range 0 1001 1))
 (check-equal? (size (delete-range 1001 2000 1 t)) 1001))

(test-case 
 "Shuffled insert/delete"
 (define num 2500) 
 (define lst (shuffle (build-list num values)))
 (define t0 (list->tree lst))
 (check-equal? (delete-range 0 num 1 t0) 'emp)
 (check-equal? (for/fold ([t t0])
                 ([v (in-list lst)])
                 (delete t v))
               'emp)
 (define t1 (insert-range 0 num 1 (delete-range 0 (- num 500) 1 t0)))
 (check-equal? (size t1) num))
