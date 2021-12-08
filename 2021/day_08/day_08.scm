(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define segments
  (map (cut string-chop <> 1)
    (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))) 

(define (segments->frequencies lst)
  (let ((mem (make-hash-table)))
    (for-each
      (lambda (char)
        (hash-table-set! mem char (+ (hash-table-ref/default mem char 0) 1)))
      (flatten lst))
    mem))

(define (segments->identifiers lst)
  (let ((freqs (segments->frequencies lst)) (mem (make-hash-table)))
    (for-each
      (lambda (segment)
        (let ((identifier (apply + (map (cut hash-table-ref freqs <>) segment))))
          (hash-table-set! mem segment identifier)))
      lst)
    mem))

(define segments-identifiers
  (let ((ids (segments->identifiers segments)) (mem (make-hash-table)))
    (for-each
      (lambda (segment identifier)
        (hash-table-set! mem (hash-table-ref ids segment) identifier))
      segments (iota (length segments)))
    mem))

(define (translate entry)
  (receive (patterns output) (apply values entry)
    (let ((ids (segments->identifiers patterns)))
      (map
        (lambda (segment)
          (hash-table-ref segments-identifiers
            (hash-table-ref ids segment)))
        output))))

(define (parse-segments str)
  (map (cut sort <> string<?)
    (map (cut string-chop <> 1) (string-split str " "))))

(define (parse-entry str)
  (map parse-segments (string-split str "|")))

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
