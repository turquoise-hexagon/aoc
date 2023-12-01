(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define-constant regex/1 "([0-9])")
(define-constant regex/2 "([0-9]|one|two|three|four|five|six|seven|eight|nine)")

(define table
  (alist->hash-table
    '(("one" .   "1")
      ("two" .   "2")
      ("three" . "3")
      ("four" .  "4")
      ("five" .  "5")
      ("six" .   "6")
      ("seven" . "7")
      ("eight" . "8")
      ("nine" .  "9"))))

(define (import-input)
  (read-lines))

(define (parse str regex)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len) '()
        (let ((match (irregex-search regex str i len)))
          (if match
            (cons (irregex-match-substring match 1) (loop (+ (irregex-match-start-index match) 1)))
            '()))))))

(define (solve input regex)
  (apply +
    (map
      (lambda (str)
        (let ((result (map (lambda (str) (hash-table-ref/default table str str)) (parse str regex))))
          (string->number (string-append (car result) (last result)))))
      input)))

(let* ((input (import-input))
       (part/1 (solve input regex/1))
       (part/2 (solve input regex/2)))
  (print part/1) (assert (= part/1 54630))
  (print part/2) (assert (= part/2 54770)))
