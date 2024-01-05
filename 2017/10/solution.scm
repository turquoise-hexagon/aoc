(import
  (chicken io)
  (chicken string)
  (srfi 1))

(include-relative "../knot-hash.scm")

(define (import-input)
  (read-line))

(define (solve input)
  (let ((vec (list->vector (iota 256))))
    (let loop ((index 0) (skip 0) (lst input))
      (if (null? lst)
        (* (vector-ref vec 0) (vector-ref vec 1))
        (let* ((value (car lst)) (limit (quotient value 2)) (next (+ index value)))
          (do ((offset 0 (+ offset 1)))
            ((= offset limit)
             (loop (+ next skip) (+ skip 1) (cdr lst)))
            (vector-swap! vec
              (modulo (+ index offset) 256)
              (modulo (- next offset 1) 256))))))))

(let ((input (import-input)))
  (let ((part/1 (solve (map string->number (string-split input ",")))))
    (print part/1) (assert (= part/1 8536)))
  (let ((part/2 (knot-hash input)))
    (print part/2) (assert (string=? part/2 "aff593797989d665349efe11bb4fd99b"))))
