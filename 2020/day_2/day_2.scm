(import (chicken io)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (string-split str ": -"))
    (read-lines (open-input-file path))))

(define (is-valid-1? input)
  (match input
         ((lower higher letter password)
          (let ((cnt (length (filter
                               (lambda (char)
                                 (string=? letter (string char)))
                               (string->list password)))))
            (not (or (< cnt (string->number lower))
                     (> cnt (string->number higher))))))))

(define (is-valid-2? input)
  (match input
         ((index-1 index-2 letter password)
          (letrec* ((helper (lambda (index)
                              (string=? letter (string (string-ref password index)))))
                    (res (not (equal? (helper (sub1 (string->number index-1)))
                                      (helper (sub1 (string->number index-2)))))))
            res))))

(define (solve proc lst)
  (display (length (filter proc lst)))
  (newline))

(let ((args (command-line-arguments)))
  (let ((lst (import-input (car args))))
    (solve is-valid-1? lst)
    (solve is-valid-2? lst)))
