(import (chicken io)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 1))

(define-record policy index-1 index-2 letter password)

(define (import-input path)
  (map
    (lambda (lst)
      (match lst
        ((index-1 index-2 letter password)
         (make-policy (string->number index-1) (string->number index-2) letter (map string (string->list password))))))
  (map (cut string-split <> ": -") (read-lines (open-input-file path)))))

(define (is-valid/1? input)
  (match input
    (($ policy index-1 index-2 letter password)
     (<= index-1 (length (filter (cut string=? letter <>) password)) index-2))))

(define (is-valid/2? input)
  (match input
    (($ policy index-1 index-2 letter password)
     (not (equal? (string=? letter (list-ref password (- index-1 1)))
                  (string=? letter (list-ref password (- index-2 1))))))))

(define (solve proc input)
  (print (length (filter proc input))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve is-valid/1? input)
    (solve is-valid/2? input)))
