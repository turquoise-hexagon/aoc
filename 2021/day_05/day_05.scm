(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define (parse-segment str)
  (map (cut map string->number <>)
    (map (cut irregex-split "," <>)
       (irregex-split " -> " str))))

(define (import-input)
  (map parse-segment (read-lines)))

(define (offset a b)
  (cond ((> a b) -1)
        ((< a b)  1)
        (else 0)))

(define (segment->points lst)
  (receive (a b) (apply values lst)
    (let ((offsets (map (cut apply offset <>) (zip a b))))
      (let loop ((t a) (acc '()))
        (let ((acc (cons t acc)))
          (if (equal? t b)
            acc
            (loop (map + t offsets) acc)))))))

(define (place-points lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (segment)
        (for-each
          (lambda (point)
            (hash-table-set! mem point (+ (hash-table-ref/default mem point 0) 1)))
          (segment->points segment)))
      lst)
    mem))

(define (solve input)
  (count (cut > <> 1) (hash-table-values (place-points input))))

(let ((input (import-input)))
  (print (solve (filter (cut apply any = <>) input)))
  (print (solve input)))
