#lang racket/base

;;; Functional implementation of a 2-3 tree
;;; TomM 2012

;;; A 2-3Tree is a self-balancing datastructure, similar to red-black and AVL trees.
;;; See http://en.wikipedia.org/wiki/2-3_tree for more information.

#| 
Todo:  
- Permit custom comparators
- Benchmarks
- tree->list
- map, fold etc.. 
|#

(require racket/match)
(require racket/contract)

(provide 
 (contract-out 
  #:∃ 23-tree
  [make-23-tree (-> 23-tree)]
  [23-tree-empty? (-> 23-tree boolean?)]
  [23-tree-find (-> 23-tree number? (or/c number? #f))]
  [23-tree-insert (-> 23-tree number? 23-tree)]
  [23-tree-delete (-> 23-tree number? 23-tree)]
  [23-tree-size (-> 23-tree integer?)]
  [list->23-tree (-> (listof number?) 23-tree)]
  
  ;; for tests
  [23-tree-balanced? (-> 23-tree (values boolean? integer?))]))

(struct 2-node (v l r))
(struct 3-node (v1 v2 l m r))

(struct split-node (v l r))
(struct hole-node (t))


(define (make-23-tree)
  'emp)

(define (23-tree-empty? tree)
  (eq? tree 'emp))

;;; --------------------------------------------------------------------------
;;; Search

(define (23-tree-find tree k)
  (match tree
    [(2-node v l r) 
     (cond [(< k v) (23-tree-find l k)]
           [(> k v) (23-tree-find r k)]
           [else k])]
    
    [(3-node v1 v2 l m r)
     (cond [(or (equal? k v1) (equal? k v2)) k]
           [(< k v1) (23-tree-find l k)]
           [(< k v2) (23-tree-find m k)]
           [else (23-tree-find r k)])]
    
    [else #f]))

;;; --------------------------------------------------------------------------
;;; Insertion
;;;
;;;

(define (singleton v)
  (2-node v 'emp 'emp))

(define (pair v1 v2)
  (3-node v1 v2 'emp 'emp 'emp))

(define (23-tree-insert tree k)
  (match (insert0 tree k)
    [(split-node v l r) (2-node v l r)]
    [t t]))  

(define (insert0 tree k)
  (bubble-splits 
   (match tree
     [(2-node v 'emp 'emp)
      (cond [(equal? k v) tree]
            [(< k v) (pair k v)]
            [else (pair v k)])]
     
     [(2-node v l r)
      (cond [(equal? k v) tree]
            [(< k v) (2-node v (insert0 l k) r)]
            [else (2-node v l (insert0 r k))])]    
     
     [(3-node v1 v2 'emp 'emp 'emp)
      (if (or (equal? k v1) (equal? k v2))
          tree
          (let-values ([(v1 v2 v3) (tri-sort k v1 v2)])
            (split-node v2 (singleton v1) (singleton v3))))]
     
     [(3-node v1 v2 l m r)
      (cond [(or (equal? k v1) (equal? k v2)) tree]
            [(< k v1) (3-node v1 v2 (insert0 l k) m r)]
            [(< k v2) (3-node v1 v2 l (insert0 m k) r)]
            [else     (3-node v1 v2 l m (insert0 r k))])]
     
     ['emp (singleton k)])))

(define (bubble-splits t)
  (match t
    [(2-node v (split-node sv sl sr) r) (3-node sv v sl sr r)]
    [(2-node v l (split-node sv sl sr)) (3-node v sv l sl sr)]
    [(3-node v1 v2 (split-node sv sl sr) m r) (split-node v1 (2-node sv sl sr) (2-node v2 m r))]
    [(3-node v1 v2 l (split-node sv sl sr) r) (split-node sv (2-node v1 l sl) (2-node v2 sr r))]
    [(3-node v1 v2 l m (split-node sv sl sr)) (split-node v2 (2-node v1 l m) (2-node sv sl sr))]
    [else t]))

;; assumes y and z are already in order
(define (tri-sort x y z)
  (cond [(< x y) (values x y z)]
        [(< x z) (values y x z)]
        [else (values y z x)]))

;;; --------------------------------------------------------------------------
;;; Deletion

(define (23-tree-delete tree k)
  (match (delete0 tree k)
    [(hole-node t) t]
    [t t]))

(define (delete0 tree k) 
  (bubble-fill-holes 
   (match tree
     [(2-node v 'emp 'emp)
      (if (equal? k v)
          (hole-node 'emp)
          tree)]
     
     [(2-node v l r)
      (cond [(equal? k v) 
             ;; swap current with a terminal node
             (define succ (smallest-member r))
             (2-node succ l (delete0 r succ))]
            [(< k v) 
             (2-node v (delete0 l k) r)]
            [else 
             (2-node v l (delete0 r k))])]
     
     [(3-node v1 v2 'emp 'emp 'emp)
      (cond [(equal? k v1) (singleton v2)]
            [(equal? k v2) (singleton v1)]
            [else tree])]
     
     [(3-node v1 v2 l m r)
      (cond [(equal? k v1)
             (define succ (smallest-member m))
             (3-node succ v2 l (delete0 m succ) r)]
            [(equal? k v2)
             (define succ (smallest-member r))
             (3-node v1 succ l m (delete0 r succ))]
            [(< k v1) 
             (3-node v1 v2 (delete0 l k) m r)]
            [(< k v2)
             (3-node v1 v2 l (delete0 m k) r)]
            [else 
             (3-node v1 v2 l m (delete0 r k))])]
     
     ['emp 'emp])))

;; todo: synthesize this automatically 
(define (bubble-fill-holes t)
  (match t
    [(2-node v (hole-node ht) (2-node rv rl rr)) 
     (hole-node (3-node v rv ht rl rr))]    
    
    [(2-node v (2-node lv ll lr) (hole-node ht)) 
     (hole-node (3-node lv v ll lr ht))]    
    
    [(2-node v (hole-node ht) (3-node rv1 rv2 rl rm rr))
     (2-node rv1 (2-node v ht rl) (2-node rv2 rm rr))]
    
    [(2-node v (3-node lv1 lv2 ll lm lr) (hole-node ht)) 
     (2-node lv2 (2-node lv1 ll lm) (2-node v lr ht))]
    
    [(3-node v1 v2 (hole-node ht) (2-node mv ml mr) r)
     (2-node v2 (3-node v1 mv ht ml mr) r)]
    
    [(3-node v1 v2 (2-node lv ll lr) (hole-node ht) r)
     (2-node v2 (3-node lv v1 ll lr ht) r)]
    
    [(3-node v1 v2 l (hole-node ht) (2-node rv rl rr))
     (2-node v1 l (3-node v2 rv ht rl rr))]
    
    [(3-node v1 v2 l (2-node mv ml mr) (hole-node ht))
     (2-node v1 l (3-node mv v2 ml mr ht))]
    
    [(3-node v1 v2 (hole-node ht) (3-node mv1 mv2 ml mm mr) r)
     (3-node mv1 v2 (2-node v1 ht ml) (2-node mv2 mm mr) r)]
    
    [(3-node v1 v2 (3-node lv1 lv2 ll lm lr) (hole-node ht) r)
     (3-node lv2 v2 (2-node lv1 ll lm) (2-node v1 lr ht) r)]
    
    [(3-node v1 v2 l (3-node mv1 mv2 ml mm mr) (hole-node ht))
     (3-node v1 mv2 l (2-node mv1 ml mm) (2-node v2 mr ht))]
    
    [else t]))

;; assumes tree is not empty
(define (smallest-member tree)
  (cond [(2-node? tree)
         (if (eq? (2-node-l tree) 'emp)
             (2-node-v tree)
             (smallest-member (2-node-l tree)))]
        [else 
         (if (eq? (3-node-l tree) 'emp)
             (3-node-v1 tree)
             (smallest-member (3-node-l tree)))]))

;;; --------------------------------------------------------------------------
;;; Helpers

(define (list->23-tree lst)
  (for/fold ([t 'emp])
    ([i (in-list lst)])
    (23-tree-insert t i)))

(define (23-tree-balanced? tree)
  (match tree
    [(2-node _ l r) 
     (define-values (bl? hl) (23-tree-balanced? l))
     (define-values (br? hr) (23-tree-balanced? r))
     (values (and bl? br? (= hl hr)) (+ hl 1))]
    
    [(3-node _ _ l m r)
     (define-values (bl? hl) (23-tree-balanced? l))
     (define-values (bm? hm) (23-tree-balanced? m))
     (define-values (br? hr) (23-tree-balanced? r))
     (values (and bl? bm? br? (= hl hm hr)) (+ hl 1))]
    
    ['emp (values #t 0)]))

(define (23-tree-size tree)
  (match tree
    [(2-node _ l r) (+ 1 (23-tree-size l) (23-tree-size r))]
    [(3-node _ _ l m r) (+ 2 (23-tree-size l) (23-tree-size m) (23-tree-size r))]
    ['emp 0]))












