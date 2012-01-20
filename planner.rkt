#lang racket

(provide fwd-search domain state)

(struct plan-domain (constants actions))
(struct action (name types params preconds effects))
(struct precond (name terms eq? val))
(struct effect (name terms val))
(struct state-var (name terms) #:transparent)

(define-syntax state
  (syntax-rules ()
    [(state sv ...) 
     (make-immutable-hash 
      `(,@(state-helper sv) ...))]))

(define-syntax state-helper
  (syntax-rules (= <=>)
    [(state-helper (n ts)) (state-helper (n ts = #t))]
    [(state-helper (n (<=> t1 t2) = v)) `(,@(state-helper (n (t1 t2) = v))
                                          ,@(state-helper (n (t2 t1) = v)))]
    [(state-helper (n ts = v)) (list (cons (state-var 'n 'ts) 'v))]))

(define-syntax domain
  (syntax-rules (constants actions preconds effects)
    [(domain (constants (t (o ...)) ...)
             (actions (n ([arg arg-type] ...) 
                         (preconds p ...) 
                         (effects e ...))
                      ...))
     (plan-domain (make-immutable-hasheq '((t . (o ...)) ...))
                  (make-immutable-hasheq
                   `((n . ,(action 'n
                                   '(arg-type ...)
                                   '(arg ...)
                                   (list (precond-helper p) ...)
                                   (list (effect-helper e) ...)))
                     ...)))]))
      
(define-syntax precond-helper
  (syntax-rules (= ~)
    [(precond-helper (~ n ts = v)) (precond 'n 'ts #f 'v)]
    [(precond-helper (n ts = v)) (precond 'n 'ts #t 'v)]))

(define-syntax effect-helper
  (syntax-rules (<-)
    [(effect-helper (n ts <- v)) (effect 'n 'ts 'v)]))

(define (hash-subset? x y)
  (for/and ([(k v) (in-hash x)])
    (eq? v (hash-ref y k #f))))

(define (bind p bs)
  (let ([b (assq p bs)])
    (if b (cdr b) p)))

(define (bind-all params bs)
  (map (λ (p) (bind p bs)) params))

(define (check-precond s p bs)
  (let ([sval (hash-ref s 
                        (state-var (precond-name p) (bind-all (precond-terms p) bs)) 
                        #f)]
        [pval (bind (precond-val p) bs)])
    (and sval (if (precond-eq? p)
                  (eq? sval pval)
                  (not (eq? sval pval))))))

;; state (listof effect) (listof bindings) -> state
(define (apply-effects s es bs)
  (foldl (λ (e s)
           (hash-set s 
                     (state-var (effect-name e)
                                (bind-all (effect-terms e) bs))
                     (bind (effect-val e) bs)))
         s
         es))

;; action (listof constant) state -> (or state #f)
(define (try-apply a args s)
  (let ([bindings (map cons (action-params a) args)])
    (if (andmap (λ (p) (check-precond s p bindings))
                (action-preconds a))
        (apply-effects s (action-effects a) bindings)
        #f)))

;; (listof (listof constant)) -> (listof (listof constant))
;; ex: ((a b) (c d)) -> ((a c) (a d) (b c) (b d))
(define (enumerate-constants lol)
  (if (empty? lol)
      '(())
      (append-map (λ (x)
                    (map (curry cons x)
                         (enumerate-constants (cdr lol))))
                  (car lol))))

;; domain action -> (listof (listof constant))
(define (action-ground-terms pd a)
  (enumerate-constants 
   (map (λ (t)
          (hash-ref (plan-domain-constants pd) t))
        (action-types a))))

;; domain state goal -> (or (listof (action-name args ...)) #f)
(define (fwd-search pd s0 g)
  (define const-tbl (plan-domain-constants pd))
  (define action-tbl (plan-domain-actions pd))
  (let fws ([s s0] [prev (set)] [acts '()])
    (cond [(set-member? prev s) #f] ;prevent loops to previous states 
          [(hash-subset? g s) (reverse acts)]
          [else 
           (for*/or ([a (in-hash-values action-tbl)]
                     [args (in-list (action-ground-terms pd a))])
             (cond [(try-apply a args s) 
                    => (λ (s2)
                         (fws s2 (set-add prev s) (cons (cons (action-name a) args) acts)))]
                   [else #f]))])))
  
  
;; Used to test a solution for validity 
(define (test-sol pd s0 g loa)
  (let ([sg (foldl (λ (a-args s)
                     (let ([s2 (try-apply (hash-ref (plan-domain-actions pd) (car a-args))
                                         (cdr a-args) 
                                         s)])
                       (or s2 s)))
                   s0
                   loa)])
    (hash-subset? g sg)))
  