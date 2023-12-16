(import
  (chicken io)
  (chicken string)
  (chicken fixnum)
  (srfi 1))

(define-constant SIZE 256)

(define (import-input)
  (string-split (read-line) ","))

(define (HASH str)
  (foldl
    (lambda (acc i)
      (fxmod (fx* (fx+ acc (char->integer i)) 17) SIZE))
    0 (string->list str)))

(define (parse str)
  (string-split str "-="))

(define (process! HASHMAP label . args)
  (let ((HASH (+ (HASH label) 1)))
    (vector-set! HASHMAP HASH
      (let ((lst (vector-ref HASHMAP HASH)))
        (apply
          (case-lambda
            (()      (alist-delete label       lst string=?))
            ((value) (alist-update label value lst string=?)))
          args)))))

(define (focusing-power HASHMAP)
  (do ((i 1 (+ i 1))
       (acc 0 (let ((lst (vector-ref HASHMAP i)))
                (fold
                  (lambda (pair index acc)
                    (+ acc (* i index (string->number (cdr pair)))))
                  acc lst (iota (length lst) 1)))))
    ((> i SIZE) acc)))

(define (solve/1 input)
  (apply + (map HASH input)))

(define (solve/2 input)
  (let ((acc (make-vector (+ SIZE 1) '())))
    (for-each
      (lambda (i)
        (apply process! acc (parse i)))
      input)
    (focusing-power acc)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 516469)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 221627))))
