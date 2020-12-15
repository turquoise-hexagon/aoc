(import (chicken io)
        (chicken process-context)
        (srfi 1))

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (get-seat-id boarding-pass)
  (define (get-seat/h lst)
    (do ((lst (reverse lst) (cdr lst))
         (cnt 0 (+ cnt 1))
         (acc 0 (case (car lst)
                  ((#\B #\R) (+ acc (expt 2 cnt))) (else acc))))
      ((null? lst) acc)))
  (+ (* (get-seat/h (take boarding-pass 7)) 8)
     (get-seat/h (drop boarding-pass 7))))

(define (solve/1 ids)
  (print (apply max ids)))

(define (solve/2 ids)
  (print (call/cc
           (lambda (return)
             (for-each
               (lambda (id)
                 (cond ((not (member (+ id 1) ids)) (return (+ id 1)))
                       ((not (member (- id 1) ids)) (return (- id 1)))))
               ids)))))

(let ((path (car (command-line-arguments))))
  (let ((ids (map get-seat-id (import-input path))))
    (solve/1 ids)
    (solve/2 ids)))
