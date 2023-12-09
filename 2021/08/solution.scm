(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define signals
  (map
    (lambda (i)
      (string-chop i 1))
    '("abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg")))

(define (signals->frequencies lst)
  (let ((mem (make-hash-table #:initial 0)))
    (for-each
      (lambda (char)
        (hash-table-update! mem char add1))
      (flatten lst))
    mem))

(define (signal->identifier freqs signal)
  (apply +
    (map
      (lambda (i)
        (hash-table-ref freqs i))
      signal)))

(define signals-identifiers
  (let ((freqs (signals->frequencies signals)) (mem (make-hash-table)))
    (for-each
      (lambda (signal number)
        (hash-table-set! mem (signal->identifier freqs signal) number))
      signals (iota (length signals)))
    mem))

(define (translate entry)
  (receive (patterns output) (apply values entry)
    (let ((freqs (signals->frequencies patterns)))
      (map
        (lambda (signal)
          (hash-table-ref signals-identifiers (signal->identifier freqs signal)))
        output))))

(define (parse-entry str)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (string-chop i 1))
        (string-split i " ")))
    (string-split str "|")))

(define (import-input)
  (map translate (map parse-entry (read-lines))))

(define (solve/1 input)
  (count
    (lambda (n)
      (case n
        ((1 4 7 8) #t)
        (else #f)))
    (flatten input)))

(define (solve/2 input)
  (apply + (map list->number input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 318)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 996280))))
