#lang racket

(require "main.rkt" rackunit)


;;; Mach-O binaries start with the magic code "cafebabe". Test consists of the first 32 bytes of a 
;;; mac-osx app.
(define magic #xCAFEBABE)     
(define test #"\312\376\272\276\0\0\0\2\1\0\0\a\200\0\0\3\0\0\20\0\0@\226\200\0\0\0\f\0\0\0\a")

(let-bytes test ([? magic : 4 unsigned/big]
                 [>> 128]
                 [next : 1]
                 [remaining rest])
  (check-equal? next 0)
  (check-equal? (bytes-length remaining) 18))

(check-exn exn:fail?
           (λ () 
             (let-bytes test ([? #xCAFEBABE : 4 signed/little])
               (void)))
           "Mismatch")

(when-bytes test ([? magic : 4 unsigned/big]
                  [x : 2 signed/big] [y : 2 unsigned/little])
            (check-equal? x 0)
            (check-equal? y #x200))

(define (make-binary-record msg)
  (define len (bytes-length (string->bytes/utf-8 msg)))
  (build-bytes [#xCAFEF00D : 4 unsigned/big]
               [len : 8 unsigned/little]
               [msg : utf-8] ;; re-converting for the sake of test coverage
               [#"OK" : bytes]
               ["DONE" : latin-1]))

(let ([message "Hello World! 私は日本の大手です"])
  (let-bytes (make-binary-record message)
    ([? #xCAFEF00D : 4 unsigned/big]
     [size : 8 unsigned/little]
     [decoded : size utf-8]
     [bs : 2 bytes]
     [? "DONE" : 4 latin-1])
    (check-equal? decoded message)
    (check-equal? bs #"OK")))
 
  

