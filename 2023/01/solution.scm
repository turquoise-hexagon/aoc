(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-constant numeric '("[0-9]"))
(define-constant textual '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(define table
  (alist->hash-table (map cons textual (map number->string (iota (length textual) 1)))))

(define (import-input)
  (read-lines))

(define (search str regex)
  (let loop ((i 0))
    (let ((match (irregex-search regex str i)))
      (if match
        (cons (irregex-match-substring match) (loop (+ (irregex-match-start-index match) 1)))
        '()))))

(define (convert str)
  (if (string->number str) str (hash-table-ref table str)))

(define (value str regex)
  (let ((result (search str regex)))
    (string->number
      (string-append
        (convert (first result))
        (convert (last  result))))))

(define (solve input lst)
  (let ((regex (string-append "(" (string-intersperse lst "|") ")")))
    (apply +
      (map
        (lambda (i)
          (value i regex))
        input))))

(let ((input (import-input)))
  (let ((part/1 (solve input numeric)))
    (print part/1) (assert (= part/1 54630)))
  (let ((part/2 (solve input (append numeric textual))))
    (print part/2) (assert (= part/2 54770))))
