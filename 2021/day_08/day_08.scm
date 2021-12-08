(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define signals '("abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))

(define (signals->frequencies lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (char)
        (hash-table-set! mem char (+ (hash-table-ref/default mem char 0) 1)))
      (flatten lst))
    mem))

(define (signals->identifiers lst)
  (let ((freqs (signals->frequencies lst)) (mem (make-hash-table)))
    (for-each
      (lambda (signal)
        (hash-table-set! mem signal
          (foldl + 0 (map (cut hash-table-ref freqs <>) signal))))
      lst)
    mem))

(define signals-identifiers
  (let ((signals (map (cut string-chop <> 1) signals))) 
    (let ((ids (signals->identifiers signals)) (mem (make-hash-table)))
      (for-each
        (lambda (signal identifier)
          (hash-table-set! mem (hash-table-ref ids signal) identifier))
        signals (iota (length signals)))
      mem)))

(define (translate entry)
  (receive (patterns output) (apply values entry)
    (let ((ids (signals->identifiers patterns)))
      (map
        (lambda (signal)
          (hash-table-ref signals-identifiers
            (hash-table-ref ids signal)))
        output))))

(define (parse-signals str)
  (map (cut sort <> string<?)
    (map (cut string-chop <> 1) (string-split str " "))))

(define (parse-entry str)
  (map parse-signals (string-split str "|")))

(define (import-input)
  (map translate (map parse-entry (read-lines))))

(define (solve/1 input)
  (count
    (lambda (i)
      (case i
        ((1 4 7 8) #t)
        (else #f)))
    (flatten input)))

(define (solve/2 input)
  (foldl + 0 (map list->number input)))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
