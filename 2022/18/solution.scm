(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define offsets
  '((-1  0  0)
    ( 0 -1  0)
    ( 0  0 -1)
    ( 1  0  0)
    ( 0  1  0)
    ( 0  0  1)))

(define (import-input)
  (alist->hash-table
    (map
      (lambda (_)
        (cons (map string->number (string-split _ ",")) #t))
      (read-lines))))

(define (neighbors coord)
  (map
    (lambda (offset)
      (map + coord offset))
    offsets))

(define (explore points)
  (let ((acc (make-hash-table)))
    (let*
      ((_ (apply zip (hash-table-keys points)))
       (mini (map (lambda (_) (- (apply min _) 1)) _))
       (maxi (map (lambda (_) (+ (apply max _) 1)) _)))
      (let loop ((lst (list mini)))
        (if (null? lst)
          acc
          (let
            ((head (car lst))
             (tail (cdr lst)))
            (if (hash-table-exists? acc head)
              (loop tail)
              (if (every <= mini head maxi)
                (if (hash-table-exists? points head)
                  (loop tail)
                  (let ((_ (neighbors head)))
                    (hash-table-set! acc head #t)
                    (loop (append _ tail))))
                (loop tail)))))))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (coord)
        (count
          (lambda (neighbor)
            (not (hash-table-exists? input neighbor)))
          (neighbors coord)))
      (hash-table-keys input))))

(define (solve/2 input)
  (let ((acc (explore input)))
    (apply +
      (map
        (lambda (coord)
          (count
            (lambda (neighbor)
              (hash-table-exists? acc neighbor))
            (neighbors coord)))
        (hash-table-keys input)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
