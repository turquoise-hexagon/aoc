(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1)
        (srfi 69))

(define-record world tuples-table h w)

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (input->world input)
  (let ((h (length input)) (w (length (list-ref input 0))) (hash (make-hash-table)))
    (do ((i 0 (+ i 1))) ((= i h))
      (do ((j 0 (+ j 1))) ((= j w))
        (hash-table-set! hash (list i j) (list-ref (list-ref input i) j))))
    (make-world hash h w)))

(define (count-occupied tuples-table)
  (count (cut char=? #\# <>) (hash-table-values tuples-table)))

(define (helper/1 tuples-table i j a b)
  (if (char=? (hash-table-ref/default tuples-table (list (+ i a) (+ j b)) #\!) #\#) 1 0))

(define (helper/2 tuples-table i j a b)
  (call/cc
    (lambda (return)
      (do ((x (+ i a) (+ x a)) (y (+ j b) (+ y b))) (#f)
        (case (hash-table-ref/default tuples-table (list x y) #\!)
          ((#\#) (return 1))
          ((#\L) (return 0))
          ((#\!) (return 0)))))))
    
(define (count-neighbors tuples-table proc i j)
  (set! cnt 0)
  (do ((a -1 (+ a 1))) ((= a 2))
    (do ((b -1 (+ b 1))) ((= b 2))
      (unless (and (= a 0) (= b 0))
        (set! cnt (+ cnt (proc tuples-table i j a b))))))
  cnt)

(define (iterate-world world-match proc setting)
  (match world-match
    (($ world tuples-table h w)
     (let ((next (hash-table-copy tuples-table)))
       (hash-table-for-each tuples-table
          (lambda (tuple value)
            (let ((cnt (apply (cut count-neighbors tuples-table proc <> <>) tuple)))
              (case value
                ((#\#) (when (> cnt setting) (hash-table-set! next tuple #\L)))
                ((#\L) (when (= cnt       0) (hash-table-set! next tuple #\#)))))))
       (make-world next h w)))))

(define (solve input proc setting)
  (let solve/h ((world (input->world input)))
    (let ((next (iterate-world world proc setting)))
      (if (equal? world next)
          (print (count-occupied (world-tuples-table world)))
          (solve/h next)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve input helper/1 3)
    (solve input helper/2 4)))
