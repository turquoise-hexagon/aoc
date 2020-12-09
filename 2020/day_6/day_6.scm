(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 113)
        (srfi 128))

(define (import-input path)
  (let ((comparator (make-default-comparator)))
    (map
      (lambda (str)
        (map (cut list->set comparator <>)
             (map string->list (irregex-split "\n" str))))
      (irregex-split "\n\n" (read-string #f (open-input-file path))))))

(define (solve proc input)
  (print (apply + (map set-size (map (cut apply proc <>) input)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve set-union        input)
    (solve set-intersection input)))
