(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define valid
  (map
    (lambda (str)
      (string-chop str 1))
    (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg")))

(define (generate-permutations)
  (let ((chars (string-chop "abcdefg" 1)))
    (map
      (lambda (permutation)
        (let ((mem (make-hash-table)))
          (for-each
            (lambda (lst)
              (apply (cut hash-table-set! mem <> <>) lst))
            (zip chars permutation))
          mem))
      (permutations chars))))

(define (generate-list permutation lst)
  (sort (map (cut hash-table-ref permutation <>) lst) string<?))

(define (find-permutation permutations patterns)
  (find
    (lambda (permutation)
      (every
        (lambda (lst)
          (member (generate-list permutation lst) valid))
        patterns))
    permutations))

(define (translate-entry permutations entry)
  (receive (patterns output) (apply values entry)
    (let ((permutation (find-permutation permutations patterns)))
      (map
        (lambda (lst)
          (list-index (cut equal? <> lst) valid))
        (map (cut generate-list permutation <>) output)))))

(define (translate-all lst)
  (let ((permutations (generate-permutations)))
    (map (cut translate-entry permutations <>) lst)))

(define (parse-entry str)
  (map
    (lambda (lst)
      (map (cut string-chop <> 1) lst))
    (map (cut string-split <> " ") (string-split str "|"))))

(define (import-input)
  (translate-all (map parse-entry (read-lines))))

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
