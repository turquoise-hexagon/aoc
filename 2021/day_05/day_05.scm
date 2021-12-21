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
  (let ((lst (map parse-segment (read-lines))))
    ;; split lst in lines and diagonals
    (partition (cut apply any = <>) lst)))

(define (segment->points lst)
  (receive (a b) (apply values lst)
    (let ((offsets (map signum (map - b a))))
      (let loop ((t a) (acc '()))
        (let ((acc (cons t acc)))
          (if (equal? t b)
            acc
            (loop (map + t offsets) acc)))))))

(define (solve mem lst)
  (for-each
    (lambda (segment)
      (for-each
        (lambda (point)
          (hash-table-set! mem point (+ (hash-table-ref/default mem point 0) 1)))
        (segment->points segment)))
    lst)
  (count (cut > <> 1) (hash-table-values mem)))

(receive (lines diags) (import-input)
  (let ((mem (make-hash-table)))
    (print (solve mem lines))
    (print (solve mem diags))))
