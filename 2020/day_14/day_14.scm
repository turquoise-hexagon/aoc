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
         (list mask
               (map
                 (lambda (instruction)
                   (match (string-split instruction "[]= ")
                     ((_ address value)
                      (list (string->number address)
                            (string->number value)))))
                 instructions)))))
    (map (cut string-split <> "\n") (irregex-split "mask = " (read-string #f (open-input-file path))))))

(define (parse-mask mask)
  (let* ((mask1 (irregex-replace/all "[01]" mask  "0"))
         (mask1 (irregex-replace/all   "X"  mask1 "1"))
         (mask2 (irregex-replace/all   "X"  mask  "0"))
         (mask1 (string->number mask1 2))
         (mask2 (string->number mask2 2)))
    (list mask1 mask2)))

(define (generate-combinations address mask)
  (let ((address (do ((acc (number->string address 2) (string-append "0" acc)))
                   ((= (string-length acc) (string-length mask)) acc))))
    (define (generate-combinations/h address mask acc)
      (if (null? mask)
          (list (list->string (reverse acc)))
          (case (car mask)
            ((#\0) (generate-combinations/h (cdr address) (cdr mask) (cons (car address) acc)))
            ((#\1) (generate-combinations/h (cdr address) (cdr mask) (cons #\1           acc)))
            ((#\X) (append (generate-combinations/h (cdr address) (cdr mask) (cons #\0 acc))
                           (generate-combinations/h (cdr address) (cdr mask) (cons #\1 acc)))))))
    (map (cut string->number <> 2) (generate-combinations/h (string->list address) (string->list mask) (list)))))

(define (proc/1 memory mask address value)
  (match (parse-mask mask)
    ((mask1 mask2)
     (hash-table-set! memory address (bitwise-ior (bitwise-and value mask1) mask2)))))

(define (proc/2 memory mask address value)
  (for-each
    (lambda (address)
      (hash-table-set! memory address value))
    (generate-combinations address mask)))

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
