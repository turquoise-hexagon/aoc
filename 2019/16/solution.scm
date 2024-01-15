(import
  (chicken fixnum)
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define (import-input)
  (map string->number (string-chop (read-line) 1)))

(define (mult n m)
  (let ((m (fxmod (fx+ m 1) (fx* 4 n))))
    (cond
      ((fx< m (fx* 1 n))  0)
      ((fx< m (fx* 2 n))  1)
      ((fx< m (fx* 3 n))  0)
      ((fx< m (fx* 4 n)) -1))))

(define (comp lst n)
  (let ((n (fx+ n 1)))
    (do ((i 0 (fx+ i 1))
         (l lst (cdr l))
         (acc 0 (fx+ acc (fx* (car l) (mult n i)))))
      ((null? l) (fxmod (fxabs acc) 10)))))

(define (proc/1 lst)
  (let loop ((i 0) (l lst))
    (if (null? l)
      '()
      (cons (comp lst i) (loop (fx+ i 1) (cdr l))))))

(define (proc/2 lst)
  (let loop ((l lst) (s (foldl fx+ 0 lst)))
    (if (null? l)
      '()
      (cons (fxmod s 10) (loop (cdr l) (fx- s (car l)))))))

(define (solve input proc)
  (do ((i 0 (+ i 1))
       (acc input (proc acc)))
    ((= i 100) (list->number (take acc 8)))))

(let* ((input/1 (import-input)) (input/2 (list-tail (join (make-list #e1e4 input/1)) (list->number (take input/1 7)))))
  (let ((part/1 (solve input/1 proc/1)))
    (print part/1) (assert (= part/1 49254779)))
  (let ((part/2 (solve input/2 proc/2)))
    (print part/2) (assert (= part/2 55078585))))
