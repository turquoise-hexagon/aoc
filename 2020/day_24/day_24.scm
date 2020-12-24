(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (matchable)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (lst)
      (map string->symbol lst))
    (map (cut irregex-extract "(e|se|sw|w|nw|ne)" <>) (read-lines (open-input-file path)))))

(define (init input)
  (let ((hash (make-hash-table)))
    (for-each
      (lambda (lst)
        (let ((coordinates
                (fold
                  (lambda (a acc)
                    (match acc
                      ((x y)
                       (case a
                         ((e)  (list (- x 2) y))
                         ((w)  (list (+ x 2) y))
                         ((se) (list (- x 1) (- y 1)))
                         ((sw) (list (+ x 1) (- y 1)))
                         ((nw) (list (+ x 1) (+ y 1)))
                         ((ne) (list (- x 1) (+ y 1)))))))
                  '(0 0) lst)))
          (if (hash-table-exists? hash coordinates)
              (hash-table-delete! hash coordinates)
              (hash-table-set!    hash coordinates 0))))
      input)
    hash))

(define (solve/1 hash)
  (print (length (hash-table-values hash))))

(define (generate-neighbors tuple)
  (fold
    (lambda (a acc)
      (cons (fold-right
              (lambda (a b acc)
                (cons (+ a b) acc))
              (list) tuple a)
            acc))
    (list) '((-2 0) (+2 0) (-1 -1) (+1 -1) (+1 +1) (-1 +1))))

(define (generate-neighbors-table hash)
  (let ((neighbors (make-hash-table)))
    (for-each
      (lambda (tuple)
        (hash-table-set! neighbors tuple (+ (hash-table-ref/default neighbors tuple 0) 1)))
      (fold
        (lambda (a acc)
          (append (delete a (generate-neighbors a)) acc))
        (list) (hash-table-keys hash)))
    neighbors))

(define (iterate-world hash)
  (let ((next (make-hash-table)))
    (hash-table-for-each (generate-neighbors-table hash)
      (lambda (tuple count)
        (if (hash-table-exists? hash tuple)
            (when (< 0 count 3)
              (hash-table-set! next tuple 0))
            (when (=   count 2)
              (hash-table-set! next tuple 0)))))
    next))

(define (solve/2 hash iter)
  (print (length (hash-table-values (fold
                   (lambda (a acc)
                     (iterate-world acc))
                   hash (iota iter))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (let ((hash (init input)))
      (solve/1 hash)
      (solve/2 hash 100))))
