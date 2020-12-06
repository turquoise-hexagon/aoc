(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (let ((hash (make-hash-table))
            (lst  (string-split str)))
        (for-each
          (lambda (char)
            (hash-table-set! hash char (add1 (hash-table-ref/default hash char 0))))
          (string->list (apply string-append lst)))
        (list hash (length lst))))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (solve/1 input)
  (display (apply + (map length (map hash-table-keys (map car input)))))
  (newline))

(define (solve/2 input)
  (display (apply + (map
                      (lambda (lst)
                        (let ((hash (car  lst))
                              (numb (cadr lst)))
                          (length (filter
                                    (lambda (key)
                                      (= (hash-table-ref hash key) numb))
                                    (hash-table-keys hash)))))
                      input)))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
