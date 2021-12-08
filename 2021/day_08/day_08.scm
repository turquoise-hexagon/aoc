(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (euler)
  (srfi 1))

(define (generate-permutations)
  (let ((chars (string-chop "abcdefg" 1)))
    ;; generate permutations on "abcdefg"
    (map (cut map cons chars <>) (permutations chars))))

(define (generate-valids)
  (let ((lst (map (cut string-chop <> 1)
               (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"))))
    (map cons lst (iota (length lst)))))

(define (generate-list permutation lst)
  ;; generate a list based on a permutation
  (sort (map cdr (map (cut assoc <> permutation) lst)) string<?))

(define (find-permutation permutations valids patterns)
  (find
    (lambda (permutation)
      (every
        (lambda (lst)
          (assoc (generate-list permutation lst) valids))
        patterns))
    permutations))

(define (translate-entry permutations valids entry)
  (receive (patterns output) (apply values entry)
    (let ((permutation (find-permutation permutations valids patterns)))
      (map cdr (map (cut assoc <> valids) (map (cut generate-list permutation <>) output))))))

(define (translate-all lst)
  (let ((permutations (generate-permutations)) (valids (generate-valids)))
    (map (cut translate-entry permutations valids <>) lst)))

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
