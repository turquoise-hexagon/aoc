(import
  (chicken io)
  (chicken string)
  (matchable)
  (srfi 69)
  (srfi 133))

(define (parse-instruction str)
  (receive (op arg) (apply values (string-split str " "))
    (list (string->symbol op) (string->number arg))))

(define (import-input)
  (list->vector (map parse-instruction (read-lines))))

(define (run-instructions vec)
  (let ((mem (make-hash-table)) (len (vector-length vec)))
    (let loop ((ptr 0) (acc 0))
      (if (or (hash-table-exists? mem ptr) (= ptr len))
        (values ptr acc)
        (begin
          (hash-table-set! mem ptr #t)
          (match (vector-ref vec ptr)
            (`(acc ,n) (loop (+ ptr 1) (+ acc n)))
            (`(jmp ,n) (loop (+ ptr n) acc))
            (`(nop ,n) (loop (+ ptr 1) acc))))))))

(define (solve/1 input)
  (nth-value 1 (run-instructions input)))

(define (solve/2 input)
  (let ((len (vector-length input)))
    (let loop ((ptr 0))
      (let ((copy (vector-copy input)))
        (match (vector-ref copy ptr)
          (`(jmp ,n) (vector-set! copy ptr `(nop ,n)))
          (`(nop ,n) (vector-set! copy ptr `(jmp ,n)))
          (_ void))
        (receive (stop acc) (run-instructions copy)
          (if (= stop len)
            acc
            (loop (+ ptr 1))))))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
