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

(define (gamma lst)
  (let ((cnt-0 (count (cut = 0 <>) lst))
        (cnt-1 (count (cut = 1 <>) lst)))
    (cond ((= cnt-0 cnt-1) 1)
          ((> cnt-0 cnt-1) 0)
          (else 1))))

(define (epsil lst)
  (if (= (gamma lst) 0) 1 0))

(define (oxy-and-co2/h lst proc)
  (let loop ((lst lst) (i 0))
    (if (= (length lst) 1)
      (car lst)
      (let ((tmp (proc (column lst i))))
        (loop (filter
                (lambda (lst)
                  (= tmp (list-ref lst i)))
                lst)
              (+ i 1))))))

(define (oxy lst) (oxy-and-co2/h lst gamma))
(define (co2 lst) (oxy-and-co2/h lst epsil))

(define (solve/1 input)
  (let ((cols (apply (cut map list <...>) input)))
    (* (list->number (map gamma cols) 2)
       (list->number (map epsil cols) 2))))

(define (solve/2 input)
  (* (list->number (oxy input) 2)
     (list->number (co2 input) 2)))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
