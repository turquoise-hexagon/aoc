(import
  (chicken bitwise)
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define (parse-instruction str)
  (let ((lst (irregex-extract "[0-9]+" str)))
    (map string->number lst)))

(define (parse-chunk str)
  (let ((lst (irregex-split "\n" str)))
    (receive (mask . instructions) (apply values lst)
      `(,mask ,(map parse-instruction instructions)))))

(define (import-input)
  (map parse-chunk (irregex-split "mask = " (read-string #f))))

(define (add-padding mask address)
  (let ((address (number->string address 2)))
    (let ((n (apply - (map string-length `(,mask ,address)))))
      (foldr string-append address (make-list n "0")))))

(define (generate mask address)
  (let ((address (add-padding mask address)))
    (let loop ((mask (string->list mask)) (address (string->list address)) (acc '()))
      (if (null? mask) `(,(string->number (list->string (reverse acc)) 2))
        (append-map
          (lambda (char)
            (loop (cdr mask) (cdr address) (cons char acc)))
          (case (car mask)
            ((#\0) `(,(car address)))
            ((#\1) `(#\1))
            ((#\X) `(#\1 #\0))))))))

(define (proc/1 memory mask address value)
  (let ((mask/1 (string->number (irregex-replace/all "X" mask "1") 2))
        (mask/2 (string->number (irregex-replace/all "X" mask "0") 2)))
    (let* ((value (bitwise-and value mask/1))
           (value (bitwise-ior value mask/2)))
      (hash-table-set! memory address value))))

(define (proc/2 memory mask address value)
  (for-each (cut hash-table-set! memory <> value)
    (generate mask address)))

(define (solve input proc)
  (let ((memory (make-hash-table)))
    (for-each
      (lambda (lst)
        (receive (mask instructions) (apply values lst)
          (for-each
            (lambda (instruction)
              (apply (cut proc memory mask <> <>) instruction))
            instructions)))
      input)
    (apply + (hash-table-values memory))))

(let ((input (import-input)))
  (print (solve input proc/1))
  (print (solve input proc/2)))
