#lang racket

(require "main.rkt")

;;; Mach-O binaries start with the magic code "cafebabe". Test consists of the first 32 bytes of a 
;;; mac-osx app.
(define magic #xCAFEBABE)     
(define test #"\312\376\272\276\0\0\0\2\1\0\0\a\200\0\0\3\0\0\20\0\0@\226\200\0\0\0\f\0\0\0\a")

(let-bytes test ([? magic : 4 unsigned/big] 
                 [x : 4 unsigned/big] [y : 1] [z : 3 bytes] [rest : 3 bytes]
                 [_ : 10] [_ : 5]
                 [end rest])
  (printf "x = ~a, y = ~a, z = ~a~n" x y (bytes->list z))
  (printf "end = ~s~n" end))


(define (make-binary-record str)
  (define msg (string->bytes/utf-8 str))
  (build-bytes [#xCAFEF00D : 4 unsigned/big]
               [(bytes-length msg) : 4 unsigned/little]
               [msg : bytes]))
  
(define encoded (make-binary-record "Hello World! 私は日本の大手です"))
(printf "Encoded message: ~s~n" encoded)

(let-bytes encoded
  ([x : 4 unsigned/big]
   [size : 4 unsigned/little]
   [msg-bytes : size utf-8])
  (printf "Decoded message: ~a~n" msg-bytes))

(define test2 (bytes-append (integer->integer-bytes #xDEADBEEF 4 #f #t)
                            #"LOL"
                            (bytes 255)))

(when-bytes test2
  ([? #xDEADBEEF : 4 unsigned/big] 
   [? #"LOL" : 3 bytes]
   [x : 1])
  (printf "When has succeded, X -> ~x~n" x))


;;; >> Can be used to fast-forward to a byte.
(define test3 (bytes-append (integer->integer-bytes #xBAAD1337 4 #f #t)
                            (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 128)
                            #"NopSledge!"))
(let-bytes test3
  ([? #xBAAD1337 : 4 unsigned/big]
   [>> 128]
   [msg rest])
  (printf "Test3 message is ~s~n" msg))

(build-bytes [#xDEADBEEF : 4 unsigned/big]
             [(bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 128) : bytes]
             ["Hello World" : utf-8])









