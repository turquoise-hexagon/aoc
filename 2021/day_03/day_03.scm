(import
  (chicken io)
  (srfi 1))

(define (import-input)
  (map string->list (read-lines)))

(define (invert lst)
  (map
    (lambda (char)
      (case char
        ((#\0) #\1)
        ((#\1) #\0)))
    lst))

(define (column lst n)
  (map (cut list-ref <> n) lst))

(define (gamma-and-epsilon/h lst proc default)
  (let ((cnt-0 (count (cut char=? #\0 <>) lst))
        (cnt-1 (count (cut char=? #\1 <>) lst)))
    (if (= cnt-0 cnt-1)
      default
      (if (proc cnt-0 cnt-1)
        #\0
        #\1))))

(define (  gamma lst) (gamma-and-epsilon/h lst > #\1))
(define (epsilon lst) (gamma-and-epsilon/h lst < #\0))

(define (oxygen-and-co2/h lst proc)
  (foldl
    (lambda (acc n)
      (if (> (length acc) 1)
        (let ((tmp (proc (column acc n))))
          (filter
            (lambda (lst)
              (char=? tmp (list-ref lst n)))
            acc))
        acc))
    lst (iota (length (car lst)))))

(define (solve/1 input)
  (let ((cols (map (cut column input <>) (iota (length (car input))))))
    (let ((  gamma (map   gamma cols))
          (epsilon (map epsilon cols)))
      (* (string->number (list->string   gamma) 2)
         (string->number (list->string epsilon) 2)))))

(define (solve/2 input)
  (let ((oxygen (car (oxygen-and-co2/h input   gamma)))
        (   co2 (car (oxygen-and-co2/h input epsilon))))
    (* (string->number (list->string oxygen) 2)
       (string->number (list->string    co2) 2))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
