(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (map (cut map string->number <>)
    (map (cut string-chop <> 1) (read-lines))))

(define (column lst n)
  (map (cut list-ref <> n) lst))

(define (gamma-and-epsil/h lst proc default)
  (let ((cnt-0 (count (cut = 0 <>) lst))
        (cnt-1 (count (cut = 1 <>) lst)))
    (if (= cnt-0 cnt-1)
      default
      (if (proc cnt-0 cnt-1)
        0
        1))))

(define (oxy-and-co2/h lst proc)
  (let oxy-and-co2/h/h ((lst lst) (i 0))
    (if (= (length lst) 1)
      (car lst)
      (let ((tmp (proc (column lst i))))
        (oxy-and-co2/h/h (filter
                           (lambda (lst)
                             (= tmp (list-ref lst i)))
                           lst)
                         (+ i 1))))))

(define (gamma lst) (gamma-and-epsil/h lst > 1))
(define (epsil lst) (gamma-and-epsil/h lst < 0))

(define (oxy lst) (oxy-and-co2/h lst gamma))
(define (co2 lst) (oxy-and-co2/h lst epsil))

(define (solve/1 input)
  (let ((cols (map (cut column input <>) (iota (length (car input))))))
    (* (list->number (map gamma cols) 2)
       (list->number (map epsil cols) 2))))

(define (solve/2 input)
  (* (list->number (oxy input) 2)
     (list->number (co2 input) 2)))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
