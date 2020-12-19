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
  (let ((h (length input)) (w (length (list-ref input 0))) (table (make-hash-table)))
    (do ((i 0 (+ i 1))) ((= i h))
      (do ((j 0 (+ j 1))) ((= j w))
        (when (char=? (list-ref (list-ref input i) j) #\#)
          (hash-table-set! table (init-tuple (list i j) size) 0))))
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

(define (generate-neighbors-table tuples-table offsets)
  (let ((neighbors (make-hash-table)))
    (for-each
      (lambda (tuple)
        (hash-table-set! neighbors tuple (+ (hash-table-ref/default neighbors tuple 0) 1)))
      (fold
        (lambda (a acc)
          (append (delete a (generate-neighbors a offsets)) acc))
        (list) (hash-table-keys tuples-table)))
    neighbors))

(define (iterate-world tuples-table offsets)
  (let ((next (make-hash-table)))
    (hash-table-for-each (generate-neighbors-table tuples-table offsets)
      (lambda (tuple count)
        (when (or      (= count 3)
                  (and (= count 2)
                       (hash-table-exists? tuples-table tuple)))
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
