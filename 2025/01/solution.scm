(import
  (chicken io)
  (chicken irregex))

(define-constant SIZE 100)
(define-constant START 50)

(define (import-input)
  (map
    (lambda (str)
      (let ((match (irregex-match "([RL])([0-9]+)" str)))
        (list
          (case (string->symbol (irregex-match-substring match 1))
            ((R) +1)
            ((L) -1))
          (string->number (irregex-match-substring match 2)))))
    (read-lines)))

(define (solve input)
  (let loop ((input input) (position START) (acc/1 0) (acc/2 0))
    (if (null? input)
      (list acc/1 acc/2)
      (apply
        (lambda (direction distance)
          (let ((next (modulo (+ position (* distance direction)) SIZE)))
            (loop (cdr input) next
              (if (= next 0)
                (+ acc/1 1)
                acc/1)
              (+ acc/2 (quotient (+ (modulo (+ SIZE (* position direction)) SIZE) distance) SIZE)))))
        (car input)))))

(let ((input (import-input)))
  (let ((_ (solve input)))
    (assert (equal? _ '(1040 6027)))
    (for-each print _)))
