#lang racket/base

(require (for-template racket/base)
         syntax/parse)
(provide skip binding test fastfwd rest-binding construction-val-annotation)


(define-syntax-class skip
  #:attributes (len)
  #:no-delimit-cut
  (pattern ((~datum _) ~! (~datum :) len:expr)))       

(define-syntax-class binding
  #:attributes (var val len)
  (pattern (var:identifier ~rest a:deconstruction-annotation)
           #:with val #'a.val
           #:with len #'a.len))

(define-syntax-class test
  #:attributes (expected val len)
  #:description "test syntax"
  (pattern ((~datum ?) expected:expr ~rest a:deconstruction-annotation)
           #:with val #'a.val
           #:with len #'a.len))

(define-syntax-class fastfwd
  #:attributes (expected)
  (pattern ((~datum >>) expected:expr)))           

(define-syntax-class rest-binding
  #:attributes (var val)
  (pattern (var:identifier (~datum rest))
           #:with val #'(subbytes bsv start blen)))

(define-syntax-class kind
  #:attributes (signed? big-endian?)
  #:description "integer kind"
  (pattern (~datum signed/big)
           #:attr signed? #t 
           #:attr big-endian? #t)
  (pattern (~datum signed/little) 
           #:attr signed? #t 
           #:attr big-endian? #f)
  (pattern (~datum unsigned/big)
           #:attr signed? #f 
           #:attr big-endian? #t)
  (pattern (~datum unsigned/little)
           #:attr signed? #f 
           #:attr big-endian? #f))

(define-syntax-class deconstruction-annotation
  #:attributes (val len)
  #:description "type annotation"
  (pattern ((~datum :) 1)
           #:with val #'(bytes-ref bsv start)
           #:with len #'1)    
  (pattern ((~datum :) len:expr (~datum bytes))
           #:with val #'(subbytes bsv start end))
  (pattern ((~datum :) len:expr (~datum utf-8))
           #:with val #'(bytes->string/utf-8 bsv #f start end))
  (pattern ((~datum :) len:expr (~datum latin-1))
           #:with val #'(bytes->string/latin-1 bsv #f start end))
  (pattern ((~datum :) len:nat k:kind)
           #:fail-unless (memq (syntax->datum #'len) '(2 4 8)) 
           "integers must be of length 2, 4, or 8 bytes"
           #:with val #`(integer-bytes->integer bsv
                                                #,(attribute k.signed?)
                                                #,(attribute k.big-endian?)
                                                start end)))

(define-syntax-class (construction-val-annotation proc)
  #:attributes (stx)
  #:description "data with type annotation"
  (pattern (val:expr (~datum :) 1)
           #:with stx #'(bytes val))
  (pattern (val:expr (~datum :) (~datum utf-8))
           #:with stx #'(string->bytes/utf-8 val))
  (pattern (val:expr (~datum :) (~datum latin-1))
           #:with stx #'(string->bytes/latin-1 val))
  (pattern (val:expr (~datum :) len:nat k:kind)
           #:fail-unless (memq (syntax->datum #'len) '(2 4 8))
           "integers must be of length 2, 4, or 8 bytes"
           #:with stx #`(integer->integer-bytes val 
                                                len
                                                #,(attribute k.signed?)
                                                #,(attribute k.big-endian?)))
  (pattern (val:expr (~datum :) (~datum bytes))
           #:with stx #`(if (bytes? val)
                            val
                            (raise-type-error '#,proc "bytes" val))))
          
  
  
  
  
  

