(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define (parse-numbers str)
  (map string->number (string-split str ",")))

(define (parse-board lst)
  (map
    (lambda (i)
      (map string->number (string-split i " ")))
    lst))

(define (import-input)
  (apply
    (lambda (numbers . boards)
      (values (parse-numbers (car numbers)) (map parse-board boards)))
    (foldr
      (lambda (i acc)
        (if (string=? i "") (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))))

(define (has called board)
  (any
    (lambda (i)
      (lset<= = i called))
    (append board (apply zip board))))

(define (unmarked called board)
  (lset-difference = (join board) called))

(define (solve/1 numbers boards)
  (let loop ((numbers numbers) (called '()))
    (let ((_ (find
               (lambda (i)
                 (has called i))
               boards)))
      (if _
        (* (apply + (unmarked called _)) (car called))
        (loop (cdr numbers) (cons (car numbers) called))))))

(define (solve/2 numbers boards)
  (let loop ((boards boards) (numbers numbers) (called '()))
    (let ((_ (remove
               (lambda (i)
                 (has called i))
               boards)))
      (if (null? _)
        (* (apply + (unmarked called (last boards))) (car called))
        (loop _ (cdr numbers) (cons (car numbers) called))))))

(let-values (((numbers boards) (import-input)))
  (let ((part/1 (solve/1 numbers boards)))
    (print part/1) (assert (= part/1 58374)))
  (let ((part/2 (solve/2 numbers boards)))
    (print part/2) (assert (= part/2 11377))))
