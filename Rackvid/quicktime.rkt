#lang racket

(require "bstruct-macro.rkt")

(provide (all-defined-out))

(define-bstruct qtheader
  ([size                (uint 32 big)]
   [type                (bytes 32)]))

(define-bstruct ftyp
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [major-brand         (bytes 32)]
   [minor-brand         (uint 32 big)]
   [compatible-brands   (vector (uint 32 big))]))

(define-bstruct mvhd
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [version             (bytes 8)]
   [flags               (bytes 24)]
   [creation-time       (uint 32 big)]
   [modification-time   (uint 32 big)]
   [time-scale          (uint 32 big)]
   [duration            (uint 32 big)]
   [preferred-rate      (q 16 16 big)]
   [preferred-volume    (q 8 8 big)]
   [reserved            (bytes 80)]
   [matrix-structure    (bytes 288)]
   [preview-time        (uint 32 big)]
   [preview-duration    (uint 32 big)]
   [poster-time         (uint 32 big)]
   [selection-time      (uint 32 big)]
   [selection-duration  (uint 32 big)]
   [current-time        (uint 32 big)]
   [next-track-id       (uint 32 big)]))

(define-bstruct tkhd
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [version             (bytes 8)]
   [flags               (bytes 24)]
   [creation-time       (uint 32 big)]
   [modification-time   (uint 32 big)]
   [trackid             (uint 32 big)]
   [reserved            (bytes 32)]
   [duration            (uint 32 big)]
   [reserved-2          (bytes 64)]
   [layer               (bytes 16)]
   [alternate-group     (bytes 16)]
   [volume              (bytes 16)]
   [reserved-3          (bytes 16)]
   [matrix-structure    (bytes 288)]
   [track-width         (q 16 16 big)]
   [track-height        (q 16 16 big)]))

(define-bstruct hdlr
  ([size                	(uint 32 big)]
   [type                	(bytes 32)]
   [version             	(bytes 8)]
   [flags               	(bytes 24)]
   [component-type      	(bytes 32)]
   [component-subtype   	(bytes 32)]
   [component-manufacturer      (bytes 32)]
   [component-flags             (bytes 32)]
   [component-flags-mask        (bytes 32)]
   [component-name              (bytes rest)]))

;; Sample description atom
(define-bstruct stsd
  ([size                        (uint 32 big)]
   [type                	(bytes 32)]
   [version             	(bytes 8)]
   [flags               	(bytes 24)]
   [entry-count         	(uint 32 big)]
   [sample-description-tbl      (bytes rest)]))

(define-bstruct stsd-entry
  ([size                (uint 32 big)]
   [format              (bytes 32)]
   [reserved            (bytes 48)]
   [data-reference-idx  (uint 16 big)]))

(define-bstruct stsd-video-entry
  ([size                (uint 32 big)]
   [format              (bytes 32)]
   [reserved            (bytes 48)]
   [data-reference-idx  (uint 16 big)]
   [version             (bytes 16)]
   [revision-level      (uint 16 big)]
   [vendor              (uint 32 big)]
   [temporal-quality    (uint 32 big)]
   [spatial-quality     (uint 32 big)]
   [width               (uint 16 big)]
   [height              (uint 16 big)]
   [horizontal-res      (uint 32 big)]
   [vertical-res        (uint 32 big)]
   [data-size           (uint 32 big)]
   [frame-count         (uint 16 big)]
   [compressor-name     (pascal-string 256)]
   [depth               (uint 16 big)]
   [color-table-id      (int 16 big)]
   [extension           (bytes rest)]))

(define-bstruct stts
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [version             (bytes 8)]
   [flags               (bytes 24)]
   [entry-count         (uint 32 big)]
   [time-to-sample-tbl  (bytes rest)]))

(define-bstruct stts-entry
  ([sample-count       (uint 32 big)]
   [sample-duration    (uint 32 big)]))

(define-bstruct ctts
  ([size                	(uint 32 big)]
   [type                	(bytes 32)]
   [version             	(bytes 8)]
   [flags               	(bytes 24)]
   [entry-count         	(uint 32 big)]
   [composition-offset-tbl      (bytes rest)]))

(define-bstruct ctts-entry
  ([sample-count        (uint 32 big)]
   [composition-offset  (int 32 big)]))

(define-bstruct stss
  ([size                 (uint 32 big)]
   [type                 (bytes 32)]
   [version              (bytes 8)]
   [flags                (bytes 24)]
   [entry-count          (uint 32 big)]
   [sync-sample-tbl      (vector (uint 32 big))]))

(define-bstruct stsc
  ([size                 (uint 32 big)]
   [type                 (bytes 32)]
   [version              (bytes 8)]
   [flags                (bytes 24)]
   [entry-count          (uint 32 big)]
   [sample-to-chunk-tbl  (bytes rest)]))

(define-bstruct stsc-entry
  ([first-chunk                 (uint 32 big)]
   [samples-per-chunk           (uint 32 big)]
   [sample-description-id       (uint 32 big)]))

(define-bstruct stsz
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [version             (bytes 8)]
   [flags               (bytes 24)]
   [sample-size         (uint 32 big)]
   [entry-count         (uint 32 big)]
   [sample-size-tbl     (vector (uint 32 big))]))

(define-bstruct stco
  ([size                (uint 32 big)]
   [type                (bytes 32)]
   [version             (bytes 8)]
   [flags               (bytes 24)]
   [entry-count         (uint 32 big)]
   [chunk-offset-tbl    (vector (uint 32 big))]))

;; returns size or eof
(define (peek-atom-size in [top-level #t])
  (define size-field (peek-bytes 4 0 in))
  (if (eof-object? size-field)
      eof
      (let ([len (integer-bytes->integer size-field #f #t)])
        (cond
         [(= len 1)
          (integer-bytes->integer (peek-bytes 8 8 in) #f #t)]
         [(and top-level (= len 0))
          (error "can't handle top level zero length yet")]
         [else len]))))

;; returns type or eof
(define (peek-atom-type in)
  (peek-bytes 4 4 in))

;; advances port past atom header
(define (skip-atom-header in)
  (file-position in (+ (file-position in) qtheader-struct-len)))

;; advances port past atom
(define (skip-current-atom in)
  (define size (peek-atom-size in))
  (unless (eof-object? size)
    (file-position in (+ (file-position in) size))))

;; bytes representing the next atom, or eof.
(define (read-current-atom in [top-level #t])
  (define size (peek-atom-size in top-level))
  (if (or (eof-object? size) (= 0 size))
      eof
      (read-bytes size in)))
