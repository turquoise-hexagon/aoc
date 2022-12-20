(import
  (chicken io)
  (srfi 1))

;; unfortunately srfi-1 does not provide these
(define (delete-at lst index)
  (let loop ((i 0) (lst lst) (acc '()))
    (if (= i index)
      (append (reverse acc) (cdr lst))
      (loop (+ i 1) (cdr lst) (cons (car lst) acc)))))

(define (insert-at lst index item)
  (let loop ((i 0) (lst lst) (acc '()))
    (if (= i index)
      (append (reverse acc) (cons item lst))
      (loop (+ i 1) (cdr lst) (cons (car lst) acc)))))

(define (import-input)
  (map string->number (read-lines)))

(define (convert lst)
  (let ((i (list-index (lambda (_) (= (car _) 0)) lst)))
    (apply +
      (map
        (lambda (_)
          (car (list-ref lst (modulo (+ i _) (length lst)))))
        '(1000 2000 3000)))))

(define (move lst index)
  (let* ((i (list-index (lambda (_) (= (cdr _) index)) lst)) (item (list-ref lst i)))
    (let*
      ((lst (delete-at lst i))
       (i (modulo (+ i (car item)) (length lst)))
       (lst (insert-at lst i item)))
      lst)))

(define (solve input iterations)
  (convert
    (foldl
      (lambda (acc _)
        (foldl move acc (iota (length input))))
      (map cons input (iota (length input))) (iota iterations))))

(let* ((input/1 (import-input)) (input/2 (map (lambda (_) (* _ 811589153)) input/1)))
  (print (solve input/1 1))
  (print (solve input/2 10)))
