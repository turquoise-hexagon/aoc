(import (chicken io)
        (chicken process-context)
        (srfi 1)
        (chicken sort))

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (get-seat boarding-pass)
  (define (get-seat/h lst lower upper)
    (if (null? lst)
        lower
        (case (car lst)
          ((#\F #\L) (get-seat/h (cdr lst) lower (/ (+ lower upper) 2)))
          (else      (get-seat/h (cdr lst) (/ (+ lower upper) 2) upper)))))
  (list (get-seat/h (take boarding-pass 7) 0 128)
        (get-seat/h (drop boarding-pass 7) 0   8)))

(define (get-seat-id boarding-pass)
  (let ((lst (get-seat boarding-pass)))
    (+ (* (car lst) 8) (cadr lst))))

(define (solve/1 ids)
  (display (apply max ids))
  (newline))

(define (solve/2 ids)
  (display
    (call/cc
      (lambda (return)
        (for-each
          (lambda (id)
            (cond ((not (member (add1 id) ids)) (return (add1 id)))
                  ((not (member (sub1 id) ids)) (return (sub1 id)))))
          ids))))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((ids (map get-seat-id (import-input path))))
    (solve/1 ids)
    (solve/2 ids)))
