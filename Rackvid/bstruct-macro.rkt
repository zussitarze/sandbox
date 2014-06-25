#lang racket

(provide define-bstruct)

(define-syntax (define-bstruct stx)
  (syntax-case stx ()
    [(define-bstruct id ([field type] ...))
     (letrec ([make-name
               (lambda (fmt . vals)
                 (datum->syntax
                  stx
                  (string->symbol
                   (apply format fmt (map syntax->datum vals)))))]
              
              [field-body
               (lambda (type offset)
                 (syntax-case type (q vector bytes pascal-string rest)
                   [(vector itype)
                    ;; capture the inner type 'fetch-function',
                    ;; repeating it to end of the struct in steps of
                    ;; the inner types' length (tlen).
                    (let-values ([(get-next-type tlen) (field-body #'itype #'o)])
                      (when (= 0 tlen)
                        (raise-syntax-error #f stx #'itype))
                      (values #`(for/vector #:length (/ (- (bytes-length the-struct) #,offset)
                                                        #,tlen)
                                            ([o (in-range #,offset (bytes-length the-struct) #,tlen)])
                                            #,get-next-type)
                              0))]
                   [(bytes rest)
                    (values #`(subbytes the-struct #,offset) 0)]
                   [(bytes bits)
                    (let ([len (/ (syntax->datum #'bits) 8)])
                      (values #`(subbytes the-struct #,offset (+ #,offset #,len))
                              len))]
                   [(q integer-bits fraction-bits end)
                    (if (and (identifier? #'end)
                             (number? (syntax->datum #'integer-bits))
                             (number? (syntax->datum #'fraction-bits))
                             (memq (syntax-e #'end) '(big little)))
                        (let* ([len (/ (+ (syntax->datum #'integer-bits)
                                          (syntax->datum #'fraction-bits))
                                       8)]
                               [big-endian? (eq? 'big (syntax-e #'end))])
                          (values #`(arithmetic-shift
                                     (integer-bytes->integer the-struct #f #,big-endian?
                                                             #,offset (+ #,offset #,len))
                                     (- #,(syntax->datum #'fraction-bits)))
                                  len))
                        (raise-syntax-error #f stx type))]
                   [(pascal-string bits)
                    (let ([len (/ (syntax->datum #'bits) 8)])
                      (values #`(let ([str-len (min (sub1 #,len)
                                                    (bytes-ref the-struct #,offset))])
                                  (bytes->string/latin-1 the-struct #f (+ 1 #,offset) (+ 1 #,offset str-len)))
                              len))]
                   [(int-kind bits end)
                    (if (and (identifier? #'int-kind)
                             (number? (syntax->datum #'bits))
                             (identifier? #'end)
                             (memq (syntax-e #'int-kind) '(uint int))
                             (memq (syntax-e #'end) '(big little)))
                        (let ([signed? (eq? 'int (syntax-e #'int-kind))]
                              [big-endian? (eq? 'big (syntax-e #'end))]
                              [len (/ (syntax->datum #'bits) 8)])
                          (values #`(integer-bytes->integer the-struct #,signed? #,big-endian?
                                                            #,offset (+ #,offset #,len))
                                  len))
                        (raise-syntax-error #f stx type))]))]
              
              [field-bodies/len
               (foldl (lambda (t acc/offset)
                        (let*-values ([(stx len) (field-body t (cdr acc/offset))])
                          (cons (cons stx (car acc/offset))
                                (+ len (cdr acc/offset)))))
                      '(() . 0)
                      (syntax->list #'(type ...)))])
       (with-syntax ([(selector ...) (map (lambda (f) (make-name "~a-~a" #'id f))
                                          (syntax->list #'(field ...)))]
                     [(selector-body ...) (reverse (car field-bodies/len))])
         #`(begin
             ;; selector bodies (todo: encourage inline?)
             (define (selector the-struct) selector-body) ...
             
             ;; returns the known fixed-length, which cannot include
             ;; the 'rest' and 'vector' portions as they are
             ;; determined dynamically.
             (define #,(make-name "~a-struct-len" #'id) #,(cdr field-bodies/len))

             ;; Print the structure (useful for debugging) 
             (define (#,(make-name "~a-print" #'id) x)
               (printf "|-- ~a --~n| Fixed size: ~a~n| Actual size: ~a~n"
                       #,(symbol->string (syntax-e #'id))
                       #,(make-name "~a-struct-len" #'id)
                       (bytes-length x))
               (printf "| ~a: ~s~n" (syntax-e #'selector) (selector x)) ...))))]))
