(import (chicken io)
        (chicken bitwise)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (matchable)
        (srfi 69))

(define (import-input path)
  (map
    (match-lambda
      ((mask . instructions)
       (list mask (map
                    (lambda (instructions)
                      (map string->number (cdr (string-split instructions "[]= "))))
                    instructions))))
    (map (cut string-split <> "\n") (irregex-split "mask = " (read-string #f (open-input-file path))))))

(define (pad address mask)
  (do ((acc (number->string address 2) (string-append "0" acc)))
    ((= (string-length  acc)
        (string-length mask))
     acc)))

(define (combinations address mask)
  (let combinations/h ((address (string->list address)) (mask (string->list mask)) (acc (string)))
    (if (null? mask)
        (list (string->number acc 2))
        (let ((proc (lambda (char) (combinations/h (cdr address) (cdr mask) (string-append acc char)))))
          (case (car mask)
            ((#\0) (proc (string (car address))))
            ((#\1) (proc                    "1"))
            ((#\X) (append (proc "0")
                           (proc "1"))))))))

(define (proc/1 memory mask address value)
  (let ((mask1 (string->number (irregex-replace/all "X" mask "1") 2))
        (mask2 (string->number (irregex-replace/all "X" mask "0") 2)))
    (hash-table-set! memory address (bitwise-ior (bitwise-and value mask1) mask2))))

(define (proc/2 memory mask address value)
  (for-each
    (lambda (address)
      (hash-table-set! memory address value))
    (combinations (pad address mask) mask)))

(define (solve proc input)
  (let ((memory (make-hash-table)))
    (for-each
      (match-lambda
        ((mask . (instructions))
         (for-each
           (match-lambda
             ((address value)
              (proc memory mask address value)))
           instructions)))
      input)
    (print (apply + (hash-table-values memory)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve proc/1 input)
    (solve proc/2 input)))
