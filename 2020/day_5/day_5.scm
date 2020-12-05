(import (chicken io)
        (chicken process-context)
        (srfi 1)
        (chicken sort))

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (get-seat boarding-pass)
  (let get-seat/h ((lst boarding-pass) (row-max 128) (row-min 0) (col-max 8) (col-min 0))
    (if (null? lst)
        (list row-min col-min)
        (case (car lst)
          ((#\F) (get-seat/h (cdr lst) (- row-max (/ (- row-max row-min) 2)) row-min col-max col-min))
          ((#\B) (get-seat/h (cdr lst) row-max (+ row-min (/ (- row-max row-min) 2)) col-max col-min))
          ((#\L) (get-seat/h (cdr lst) row-max row-min (- col-max (/ (- col-max col-min) 2)) col-min))
          ((#\R) (get-seat/h (cdr lst) row-max row-min col-max (+ col-min (/ (- col-max col-min) 2))))))))

(define (get-seat-id boarding-pass)
  (let ((lst (get-seat boarding-pass)))
    (+ (* (car lst) 8) (cadr lst))))

(define (solve/1 input)
  (display (apply max (map get-seat-id input)))
  (newline))

(define (solve/2 input)
  (let ((ids (map get-seat-id input)))
    (display
      (call/cc
        (lambda (return)
          (for-each
            (lambda (id)
              (cond ((not (member (add1 id) ids)) (return (add1 id)))
                    ((not (member (sub1 id) ids)) (return (sub1 id)))))
            ids))))
    (newline)))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
