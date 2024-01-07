(import
  (chicken io)
  (chicken sort)
  (euler)
  (srfi 1))

(define (import-input)
  (map string->list (read-lines)))

(define (solve/1 input)
  (let ((l (map
             (lambda (i)
               (run-length (sort i char<?) char=?))
             input)))
    (* (count (lambda (i) (any (lambda (i) (= (car i) 2)) i)) l)
       (count (lambda (i) (any (lambda (i) (= (car i) 3)) i)) l))))

(define (solve/2 input)
  (let loop ((lst (combinations input 2)))
    (apply
      (lambda (a b)
        (if (= (length a)
               (length b)
               (+ (count char=? a b) 1))
          (fold
            (lambda (a b acc)
              (if (char=? a b)
                (string-append acc (string a))
                acc))
            "" a b)
          (loop (cdr lst))))
      (car lst))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 4920)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "fonbwmjquwtapeyzikghtvdxl"))))
