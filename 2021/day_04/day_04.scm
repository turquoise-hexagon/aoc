(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (parse-numbers str)
  (map string->number (irregex-split "," str)))

(define (parse-board str)
  (map (cut map string->number <>)
    (map (cut irregex-split " " <>) (irregex-split "\n" str))))

(define (import-input)
  (let ((lst (irregex-split "\n{2}" (read-string #f))))
    (receive (numbers . boards) (apply values lst)
      (list (parse-numbers numbers) (map parse-board boards)))))

(define (rotate board)
  (apply (cut map list <...>) board))

(define (has numbers board)
  (any (cut lset<= = <> numbers) (append board (rotate board))))

(define (unmarked numbers board)
  (lset-difference = (flatten board) numbers))

(define (solve/1 input)
  (receive (numbers boards) (apply values input)
    (let loop ((numbers numbers) (acc '()))
      (let ((res (find (cut has acc <>) boards)))
        (if res
          (* (apply + (unmarked acc res)) (car acc))
          (loop (cdr numbers) (cons (car numbers) acc)))))))

(define (solve/2 input)
  (receive (numbers boards) (apply values input)
    (let loop ((numbers numbers) (boards boards) (acc '()))
      (let ((res (remove (cut has acc <>) boards)))
        (if (null? res)
          (* (apply + (unmarked acc (last boards))) (car acc))
          (loop (cdr numbers) res (cons (car numbers) acc)))))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
