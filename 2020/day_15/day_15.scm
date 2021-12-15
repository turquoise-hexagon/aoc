(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (import-input)
  (map string->number (string-split (read-line) ",")))

(define (solve input n)
  (let ((len (length input)) (mem (make-vector n #f)))
    (for-each (cut vector-set! mem <> <>) input (iota len 1))
    (let loop ((turn len) (last (last input)))
      (if (= turn n) last
        (let ((new (let ((res (vector-ref mem last)))
                     (if res (- turn res)
                       0))))
          (vector-set! mem last turn)
          (loop (+ turn 1) new))))))

(let ((input (import-input)))
  (print (solve input 2020))
  (print (solve input 30000000)))
