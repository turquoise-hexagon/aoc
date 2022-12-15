(import
  (chicken io)
  (chicken string)
  (chicken sort)
  (srfi 1))

(define (distance a b)
  (apply + (map abs (map - a b))))

(define (parse-sensor str)
  (apply
    (lambda (a b)
      (cons (distance a b) a))
    (let ((_ (string-split str " =,:")))
      (chop (filter-map string->number _) 2))))

(define (import-input)
  (map parse-sensor (read-lines)))

(define (process-sensor lst n)
  (apply
    (lambda (d a b)
      (let ((_ (- d (abs (- b n)))))
        (if (> _ 0)
          (list (- a _)
                (+ a _))
          #f))) lst))

(define (generate-ranges lst n)
  (sort
    (filter-map
      (lambda (sensor)
        (process-sensor sensor n))
      lst)
    (lambda (a b)
      (< (car a)
         (car b)))))

(define (merge-ranges lst)
  (let loop ((lst lst) (m '_) (n '_) (acc '()))
    (if (null? lst)
      (cons (list m n) acc)
      (apply
        (lambda (a b)
          (if (and (number? m)
                   (number? n))
            (if (>= n a)
              (loop (cdr lst) m (max n b) acc)
              (loop (cdr lst) a b (cons (list m n) acc)))
            (loop (cdr lst) a b acc)))
        (car lst)))))

(define (find-spaces lst n)
  (merge-ranges (generate-ranges lst n)))

(define (solve/1 input)
  (apply +
    (map
      (lambda (lst)
        (apply - (reverse lst)))
      (find-spaces input #e2e6))))

(define (solve/2 input)
  (let loop ((i 0))
    (let ((_ (find-spaces input i)))
      (if (null? (cdr _))
        (loop (+ i 1))
        (apply
          (lambda (a b)
            (+ (* (+ b 1) #e4e6) i))
          (car (reverse _)))))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
