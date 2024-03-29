(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (map (cut map string->number <>)
    (map (cut string-chop <> 1) (read-lines))))

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
      ;; run proc on ith column from lst
      (let ((tmp (proc (map (cut list-ref <> i) lst))))
        (loop (filter
                (lambda (lst)
                  (= tmp (list-ref lst i)))
                lst)
              (+ i 1))))))

(define (oxy lst) (oxy-and-co2/h lst gamma))
(define (co2 lst) (oxy-and-co2/h lst epsil))

(define (solve/1 input)
  ;; get columns from input
  (let ((cols (apply zip input)))
    (* (list->number (map gamma cols) 2)
       (list->number (map epsil cols) 2))))

(define (solve/2 input)
  (* (list->number (oxy input) 2)
     (list->number (co2 input) 2)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1025636)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 793873))))
