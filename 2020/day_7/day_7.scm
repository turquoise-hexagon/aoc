(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 69))

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

(define (lst->hash lst)
  (let ((hash (make-hash-table)))
    (for-each
      (lambda (lst)
        (let ((key (cadr lst))
              (val (car  lst)))
          (unless (string=? key "no other")
            (hash-table-set! hash key val))))
      lst)
    hash))

(define (import-input path)
  (let ((hash (make-hash-table)))
    (for-each
      (lambda (lst)
        (let ((key (cadar lst))
              (val (lst->hash (cdr lst))))
          (hash-table-set! hash key val)))
      (map parse-line (read-lines (open-input-file path))))
    hash))

(define (solve/1 input color)
  (define (solve/1/h hash)
    (let ((keys (hash-table-keys hash)))
      (hash-table-for-each input
                           (lambda (key/i hash/i)
                             (for-each
                               (lambda (key)
                                 (when (hash-table-exists? hash/i key)
                                   (hash-table-set! hash key/i 0)))
                               keys)))
      (if (equal? keys (hash-table-keys hash)) keys (solve/1/h hash))))
  (display (sub1 (length (solve/1/h (alist->hash-table `((,color . 0)))))))
  (newline))

(define (solve/2 input color)
  (define (solve/2/h color)
    (let ((hash (hash-table-ref input color)))
      (if (null? (hash-table-keys hash))
          1
          (apply + (cons 1 (hash-table-map hash
                                           (lambda (key hash)
                                             (* hash (solve/2/h key)))))))))
  (display (sub1 (solve/2/h color)))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input "shiny gold")
    (solve/2 input "shiny gold")))
