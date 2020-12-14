(import (chicken io)
        (chicken bitwise)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (matchable)
        (srfi 69))

(define (import-input path)
  (map
    (lambda (lst)
      (match lst
        ((mask . instructions)
         (list mask (map
                      (lambda (instructions)
                        (map string->number (cdr (string-split instructions "[]= "))))
                      instructions)))))
    (map (cut string-split <> "\n") (irregex-split "mask = " (read-string #f (open-input-file path))))))

(define (parse-mask mask)
  (let* ((mask1 (irregex-replace/all "X"  mask "1"))
         (mask2 (irregex-replace/all "X"  mask "0")))
    (map (cut string->number <> 2) (list mask1 mask2))))

(define (combinations address mask)
  (let ((address (do ((acc (number->string address 2) (string-append "0" acc))) ((= (string-length acc) (string-length mask)) acc))))
    (map (cut string->number <> 2)
         (let combinations/h ((address (string->list address)) (mask (string->list mask)) (acc (list)))
           (if (null? mask)
               (list (list->string (reverse acc)))
               (case (car mask)
                 ((#\0) (combinations/h (cdr address) (cdr mask) (cons (car address) acc)))
                 ((#\1) (combinations/h (cdr address) (cdr mask) (cons #\1           acc)))
                 ((#\X) (append (combinations/h (cdr address) (cdr mask) (cons #\0 acc))
                                (combinations/h (cdr address) (cdr mask) (cons #\1 acc))))))))))

(define (proc/1 memory mask address value)
  (match (parse-mask mask)
    ((mask1 mask2)
     (hash-table-set! memory address (bitwise-ior (bitwise-and value mask1) mask2)))))

(define (proc/2 memory mask address value)
  (for-each
    (lambda (address)
      (hash-table-set! memory address value))
    (combinations address mask)))

(define (solve proc input)
  (let ((memory (make-hash-table)))
    (for-each
      (lambda (lst)
        (match lst
          ((mask . (instructions))
           (for-each
             (lambda (instruction)
               (match instruction
                 ((address value)
                  (proc memory mask address value))))
              instructions))))
      input)
    (print (apply + (hash-table-values memory)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve proc/1 input)
    (solve proc/2 input)))
