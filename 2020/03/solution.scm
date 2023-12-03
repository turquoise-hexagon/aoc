(import
  (chicken io))

(define (import-input)
  (map string->list (read-lines)))

(define (encountered-trees input w h y x)
  (do ((i 0 (+ i x))
       (j 0 (+ j y))
       (acc 0 (if (char=? (list-ref (list-ref input i) (modulo j w)) #\#)
                (+ acc 1)
                acc)))
    ((>= i h) acc)))

(define (solve input offsets)
  (let ((h (length input)) (w (length (car input))))
    (apply *
      (map
        (lambda (offset)
          (apply encountered-trees input w h offset))
        offsets))))

(let ((input (import-input)))
  (let ((part/1 (solve input '((3 1)))))
    (print part/1) (assert (= part/1 242)))
  (let ((part/2 (solve input '((1 1) (3 1) (5 1) (7 1) (1 2)))))
    (print part/2) (assert (= part/2 2265549792))))
