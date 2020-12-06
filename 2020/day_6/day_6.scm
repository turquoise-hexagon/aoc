(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (srfi 69))

(define (import-input path)
  (map
    (lambda (str)
      (let ((hash  (make-hash-table))
            (split (string-split str)))
        (for-each
          (lambda (char)
            (hash-table-set! hash char (add1 (hash-table-ref/default hash char 0))))
          (string->list (apply string-append split)))
        (list hash (length split))))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (solve/1 input)
  (display (apply + (map length (map hash-table-keys (map car input)))))
  (newline))

(define (solve/2 input)
  (display (apply + (map
                      (lambda (lst)
                        (let ((hash (car  lst))
                              (numb (cadr lst)))
                          (apply + (map
                                     (lambda (key)
                                       (if (= (hash-table-ref/default hash key 0) numb) 1 0))
                                     (hash-table-keys hash)))))
                      input)))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
