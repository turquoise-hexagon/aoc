(import
  (chicken string)
  (srfi 1))

(define (import-input)
  (let
    ((nb-children (read))
     (nb-metadata (read)))
    (cons
      (foldr (lambda (_ acc) (cons (import-input) acc)) '() (iota nb-children))
      (foldr (lambda (_ acc) (cons         (read) acc)) '() (iota nb-metadata)))))

(define (solve/1 node)
  (let
    ((children (car node))
     (metadata (cdr node)))
    (apply + (append (map solve/1 children) metadata))))

(define (solve/2 node)
  (let
    ((children (car node))
     (metadata (cdr node)))
    (apply +
      (if (null? children)
        metadata
        (map
          (lambda (i)
            (if (<= 1 i (length children))
              (solve/2 (list-ref (reverse children) (- i 1)))
              0))
          metadata)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 47464)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 23054))))
