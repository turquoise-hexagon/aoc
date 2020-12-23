(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 69)
        (srfi 1))

(define (parse-line line)
  (map
    (lambda (str)
      (let ((match (irregex-match "^([0-9]*) ?(.*)$" str)))
        (let* ((num (irregex-match-substring match 1))
               (col (irregex-match-substring match 2))
               (num (let ((tmp (string->number num)))
                      (if tmp tmp 1))))
          (cons col num))))
    (irregex-split "(, | contain | bags?|\\.)" line)))

(define (line->alist line)
  (let ((lst (parse-line line)))
    (cons (caar lst) (alist->hash-table (filter
                                          (lambda (lst)
                                            (not (string=? (car lst) "no other")))
                                          (cdr lst))))))

(define (import-input path)
  (alist->hash-table (map line->alist (read-lines (open-input-file path)))))

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
      (if (equal? keys (hash-table-keys hash))
          keys
          (solve/1/h hash))))
  (print (- (length (solve/1/h (alist->hash-table `((,color . 0))))) 1)))

(define (solve/2 input color)
  (define (solve/2/h color)
    (let ((hash (hash-table-ref input color)))
      (if (null? (hash-table-keys hash))
          1
          (apply + (cons 1 (hash-table-map hash
                             (lambda (key hash)
                               (* hash (solve/2/h key)))))))))
  (print (- (solve/2/h color) 1)))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input "shiny gold")
    (solve/2 input "shiny gold")))
