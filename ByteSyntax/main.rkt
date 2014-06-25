#lang racket/base

(require (for-syntax racket/base 
                     syntax/parse 
                     "syntax-classes.rkt"))
(provide let-bytes when-bytes build-bytes)

(define-syntax (let-bytes stx)
  (define-splicing-syntax-class maybe-handler
    #:attributes (handler)
    (pattern (~optional (~seq #:error-handler handler:expr)
                        #:defaults ([handler #'(λ (msg) (raise-user-error 'let-bytes msg))]))))
  
  (define parse-clause 
    (syntax-parser #:context stx
                   [((rb:rest-binding) body ...)
                    #'(let ([rb.var rb.val])
                        body ...)]
                   [((b:binding clause ...) body ...)
                    #`(let* ([end (+ start b.len)]
                             [b.var b.val]
                             [start end])
                        #,(parse-clause #'((clause ...) body ...)))]
                   [((t:test clause ...) body ...)
                    #`(let* ([end (+ start t.len)]
                             [testval t.val]
                             [start end])
                        (if (equal? t.expected testval)
                            #,(parse-clause #'((clause ...) body ...))
                            (handler (format "expected to match ~s, but read ~s" 
                                             t.expected testval))))]
                   [((s:skip clause ...) body ...)
                    #`(let ([start (+ start s.len)])
                        #,(parse-clause #'((clause ...) body ...)))]     
                   [((f:fastfwd clause ...) body ...)
                    #`(let ([expected f.expected])
                        (let fwd ([start start])
                          (cond [(>= start blen)
                                 (handler (format "Failed to find byte ~s in sequence" expected))]
                                [(equal? expected (bytes-ref bsv start))
                                 (let ([start (+ start 1)])
                                   #,(parse-clause #'((clause ...) body ...)))]
                                [else (fwd (+ start 1))])))]
                   [(() body ...) 
                    #'(begin body ...)]))
  
  (syntax-parse stx
    [(_ bs err:maybe-handler (clause ...) body ...+)
     #`(let ([handler err.handler])
         (with-handlers ([exn:fail:contract? 
                          (λ (e) (handler (exn-message e)))])
           (let* ([bsv bs]
                  [blen (bytes-length bsv)]
                  [start 0])
             #,(parse-clause #'((clause ...) body ...)))))]))

(define-syntax (build-bytes stx)
  (syntax-parse stx
    [(_ (~var x (construction-val-annotation 'build-bytes)) ...)
     #'(bytes-append x.stx ...)]))
        
(define-syntax-rule (when-bytes bs (clause ...) body ...)
  (let-bytes bs #:error-handler (λ (_) (void))
    (clause ...) body ...))












