(import
  (chicken io)
  (euler))

(define (import-input)
  (map string->number (read-lines)))

(define (solve subject door card mod)
  (expt-mod card (discrete-log subject door mod) mod))

(receive (door card) (apply values (import-input))
  (print (solve 7 door card 20201227)))
