(import
  (chicken io)
  (chicken string)
  (srfi 134))

(define (import-input)
  (apply
    (lambda (nb-players _ _ _ _ _ last-marble _)
      (values
        (string->number nb-players)
        (string->number last-marble)))
    (string-split (read-line) " ")))

(define (ideque-rotate deque n)
  (define (main rem add sel)
    (let loop ((i 0) (acc deque))
      (if (= i (abs n))
        acc
        (loop (+ i 1) (add (rem acc) (sel acc))))))
  (if (< n 0)
    (main ideque-remove-front ideque-add-back  ideque-front)
    (main ideque-remove-back  ideque-add-front ideque-back)))

(define (solve nb-players last-marble)
  (let ((acc (make-vector (+ nb-players 1) 0)))
    (let loop ((i 1) (deque (ideque 0)))
      (if (> i last-marble)
        (apply max (vector->list acc))
        (loop (+ i 1)
          (if (zero? (modulo i 23))
            (let* ((deque (ideque-rotate deque  7))
                   (value (ideque-back deque)) (deque (ideque-remove-back deque))
                   (deque (ideque-rotate deque -1)))
              (vector-set! acc (modulo i nb-players) (+ (vector-ref acc (modulo i nb-players)) i value))
              deque)
            (ideque-add-back (ideque-rotate deque -1) i)))))))

(let-values (((nb-players last-marble) (import-input)))
  (let ((part/1 (solve nb-players (* last-marble 1))))
    (print part/1) (assert (= part/1 388844)))
  (let ((part/2 (solve nb-players (* last-marble 100))))
    (print part/2) (assert (= part/2 3212081616))))
