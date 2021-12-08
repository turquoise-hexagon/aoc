(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define signals
  (map (cut string-chop <> 1)
    '("abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg")))

(define (signals->frequencies lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (char)
        (hash-table-set! mem char (+ (hash-table-ref/default mem char 0) 1)))
      (flatten lst))
    mem))

(define (signal->identifier freqs signal)
  (foldl + 0 (map (cut hash-table-ref freqs <>) signal)))

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
    (lambda (lst)
      (map (cut string-chop <> 1) lst))
    (map (cut string-split <> " ") (string-split str "|"))))

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
  (foldl + 0 (map list->number input)))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
