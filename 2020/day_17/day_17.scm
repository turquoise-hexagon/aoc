(import (chicken io)
        (chicken process-context)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (permutations lst size)
  (if (= size 0)
      '(())
      (append-map
        (lambda (a)
          (map (cut cons <> a) lst))
        (permutations lst (- size 1)))))

(define (init-tuple lst size)
  (append lst (iota (- size (length lst)) 0 0)))

(define (input->tuples-table input size)
  (let ((table (make-hash-table)))
    (for-each
      (lambda (i)
        (for-each
          (lambda (j)
            (when (char=? (list-ref (list-ref input i) j) #\#)
              (hash-table-set! table (init-tuple (list i j) size) 0)))
          (iota (length (list-ref input 0)))))
      (iota (length input)))
    table))

(define (generate-neighbors tuple offsets)
  (fold
    (lambda (a acc)
      (cons (fold-right
              (lambda (a b acc)
                (cons (+ a b) acc))
              (list) tuple a)
            acc))
    (list) offsets))

(define (iterate-world tuples-table offsets)
  (let ((next (make-hash-table)) (neighbors (make-hash-table)))
    (for-each
      (lambda (tuple)
        (hash-table-set! neighbors tuple (+ (hash-table-ref/default neighbors tuple 0) 1)))
      (fold
        (lambda (a acc)
          (append (filter
                    (lambda (b)
                      (not (equal? a b)))
                    (generate-neighbors a offsets))
                  acc))
        (list) (hash-table-keys tuples-table)))
    (hash-table-for-each neighbors
      (lambda (tuple count)
        (when (or (and (hash-table-exists? tuples-table tuple) (= count 2)) (= count 3))
          (hash-table-set! next tuple 0))))
    next))

(define (solve input size iter)
  (let ((tuples-table (input->tuples-table input size)) (offsets (permutations '(-1 0 1) size)))
    (print (length (hash-table-keys (fold
                                      (lambda (a acc)
                                        (iterate-world acc offsets))
                                      tuples-table (iota iter)))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input 3 6)
    (solve input 4 6)))
