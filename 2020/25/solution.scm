(import
  (chicken io)
  (euler))

(define (import-input)
  (apply values (map string->number (read-lines))))

(define (solve subject door card mod)
  (modular-expt card (discrete-log subject door mod) mod))

(let-values (((door card) (import-input)))
  (let ((part (solve 7 door card 20201227)))
    (print part) (assert (= part 711945))))
