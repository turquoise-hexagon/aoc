(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 69))

(import (chicken sort))

(define (parse-line line)
  (map
    (lambda (str)
      (let ((match (irregex-match "^([0-9]*) ?(.*)$" str)))
        (let* ((num (irregex-match-substring match 1))
               (col (irregex-match-substring match 2))
               (num (let ((tmp (string->number num)))
                      (if tmp tmp 1))))
          (list num col))))
    (irregex-split "(, | contain | bags?|\\.)" line)))

(define (import-input path)
  (let ((hash (make-hash-table)))
    (for-each
      (lambda (lst)
        (let ((key (cadar lst))
              (val (let ((tmp (make-hash-table)))
                     (for-each
                       (lambda (lst)
                         (let ((key (cadr lst))
                               (val (car  lst)))
                           (unless (string=? key "no other")
                             (hash-table-set! tmp key val))))
                       (cdr lst))
                     tmp)))
          (hash-table-set! hash key val)))
      (map parse-line (read-lines (open-input-file path))))
    hash))

(define (solve/1 input color)
  (define (solve/1/h key)
    (let ((hash (hash-table-ref input key)))
      (cond ((null? (hash-table-keys hash))
             0)
            ((not (null? (hash-table-ref/default hash color (list))))
             1)
            (else
             (if (member 1 (map solve/1/h (hash-table-keys hash))) 1 0)))))
  (display (apply + (map solve/1/h (hash-table-keys input))))
  (newline))

(define (solve/2 input color)
  (define (solve/2/h color)
    (let ((hash (hash-table-ref input color)))
      (if (null? (hash-table-keys hash))
          1
          (apply + (cons 1 (map
                             (lambda (key)
                               (* (hash-table-ref hash key) (solve/2/h key)))
                             (hash-table-keys hash)))))))
  (display (sub1 (solve/2/h color)))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input "shiny gold")
    (solve/2 input "shiny gold")))
