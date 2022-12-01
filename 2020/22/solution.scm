(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (matchable)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (map string->number (cdr (string-split str "\n"))))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (compute-score lst)
  (fold
    (lambda (a b acc)
      (+ acc (* a b)))
    0 (reverse lst) (iota (length lst) 1 1)))

(define (play-combat/1 a b)
  (let play-combat/1/h ((a a) (b b))
    (cond ((null? a) (list 'b b))
          ((null? b) (list 'a a))
          ((> (car a) (car b)) (play-combat/1/h (append (cdr a) (list (car a) (car b))) (cdr b)))
          ((> (car b) (car a)) (play-combat/1/h (cdr a) (append (cdr b) (list (car b) (car a))))))))

(define (play-combat/2 a b)
  (let ((memory (make-hash-table)))
    (let play-combat/2/h ((a a) (b b))
      (if (hash-table-exists? memory (list a b))
          (list 'a a)
          (begin
            (hash-table-set! memory (list a b) 0)
            (cond ((null? a) (list 'b b))
                  ((null? b) (list 'a a))
                  ((and (<= (car a) (length (cdr a)))
                        (<= (car b) (length (cdr b))))
                   (match (play-combat/2 (take (cdr a) (car a))
                                         (take (cdr b) (car b)))
                     ((winner _)
                      (case winner
                        ((a) (play-combat/2/h (append (cdr a) (list (car a) (car b))) (cdr b)))
                        ((b) (play-combat/2/h (cdr a) (append (cdr b) (list (car b) (car a)))))))))
                  ((> (car a) (car b)) (play-combat/2/h (append (cdr a) (list (car a) (car b))) (cdr b)))
                  ((> (car b) (car a)) (play-combat/2/h (cdr a) (append (cdr b) (list (car b) (car a)))))))))))

(define (solve proc input)
  (match-let*
    (((a b) input)
     ((_ deck) (proc a b)))
    (print (compute-score deck))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve play-combat/1 input)
    (solve play-combat/2 input)))
