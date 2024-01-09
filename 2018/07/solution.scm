(import
  (chicken io)
  (chicken sort)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (let
          ((a (string-ref i  5))
           (b (string-ref i 36)))
          (unless (hash-table-exists? acc a) (hash-table-set! acc a '()))
          (unless (hash-table-exists? acc b) (hash-table-set! acc b '()))
          (hash-table-update! acc a (lambda (_) (cons b _)))))
      (read-lines))
    acc))

(define (solve/1 input)
  (list->string
    (topological-sort
      (sort
        (hash-table-map input cons)
        (lambda (a b)
          (string<?
            (list->string a)
            (list->string b))))
      char=?)))

(define (lookup input mem)
  (find
    (lambda (n)
      (and (not (any
                  (lambda (i)
                    (member n i))
                  (hash-table-values input)))
           (not (any
                  (lambda (i)
                    (and i (char=? (car i) n)))
                  (vector->list mem)))))
    (hash-table-keys input)))

(define (solve/2 input nb)
  (let ((mem (make-vector nb #f)))
    (do ((acc -1 (+ acc 1)))
      ((zero? (hash-table-size input)) acc)

      (do ((i 0 (+ i 1))) ((= i nb))
        (let ((v (vector-ref mem i)))
          (when (and v (= (cdr v) acc))
            (hash-table-delete! input (car v))
            (vector-set! mem i #f)))

       (do ((i 0 (+ i 1))) ((= i nb))
         (unless (vector-ref mem i)
           (let ((n (lookup input mem)))
             (when n (vector-set! mem i (cons n (+ acc (- (char->integer n) 4))))))))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (string=? part/1 "AHJDBEMNFQUPVXGCTYLWZKSROI")))
  (let ((part/2 (solve/2 input 5)))
    (print part/2) (assert (= part/2 1031))))
