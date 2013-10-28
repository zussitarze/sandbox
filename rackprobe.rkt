#lang racket/base

(require "quicktime.rkt")

(provide go)

(define movie-path (build-path "testvideo.mov"))

(define (go)
  (call-with-input-file movie-path read-header))

(define (read-header in)
  (let loop ()
    (define current-type (peek-atom-type in))
    (unless (eof-object? current-type)
      (printf "*** ~a (~a) ***~n" current-type (peek-atom-size in))      
      (case current-type
        [(#"moov" #"trak" #"mdia" #"minf" #"stbl")
         (skip-atom-header in)]
        [(#"mvhd")
         (define atom (read-current-atom in))
         (displayln (exact->inexact (/ (mvhd-duration atom)
                                       (mvhd-time-scale atom))))]
        [(#"tkhd")
         (define atom (read-current-atom in))
         (tkhd-print atom)]
        [(#"hdlr")
         (define atom (read-current-atom in))
         (hdlr-print atom)]
        [(#"stsd")
         (define atom (read-current-atom in))
         (define entries (stsd-entry-count atom))
         (define tbl-port (open-input-bytes (stsd-sample-description-tbl atom)))
         (for ([i (in-range entries)])
              (define size (peek-atom-size tbl-port))
              (define entry (read-bytes size tbl-port))
              (stsd-video-entry-print entry)
              (for ([atom (in-port (lambda (in) (read-current-atom in #f))
                                   (open-input-bytes (stsd-video-entry-extension entry)))])
                   (printf "s:~a t:~a~n" (qtheader-size atom) (qtheader-type atom))))]
	[else (skip-current-atom in)])
      (loop))))
