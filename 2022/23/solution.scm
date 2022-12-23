(import
  (chicken io)
  (srfi 1)
  (srfi 69))

(define N  '(-1  0))
(define NE '(-1  1))
(define E  '( 0  1))
(define SE '( 1  1))
(define S  '( 1  0))
(define SW '( 1 -1))
(define W  '( 0 -1))
(define NW '(-1 -1))

(define offsets
  (list
    (list N NE NW)
    (list S SE SW)
    (list W NW SW)
    (list E NE SE)))

(define (parse-input lst)
  (let ((acc (make-hash-table)))
    (let loop ((i lst) (coord '()))
      (if (list? i)
        (for-each
          (lambda (i index)
            (loop i (cons index coord)))
          i (iota (length i)))
        (case i ((#\#) (hash-table-set! acc (reverse coord) #t)))))
    acc))

(define (import-input)
  (parse-input (map string->list (read-lines))))

(define (has-neighbor? table coord #!optional (offsets (list N NE E SE S SW W NW)))
  (any
    (lambda (next)
      (hash-table-exists? table next))
    (map
      (lambda (offset)
        (map + coord offset))
      offsets)))

(define (test-offsets table coord offsets)
  (if (has-neighbor? table coord offsets)
    #f
    (list coord (map + coord (car offsets)))))

(define (suggest-move table coord offsets)
  (if (has-neighbor? table coord)
    (let loop ((lst offsets))
      (if (null? lst)
        #f
        (let ((result (test-offsets table coord (car lst))))
          (if result result (loop (cdr lst))))))
    #f))

(define (get-all-moves table offsets)
  (filter-map
    (lambda (coord)
      (suggest-move table coord offsets))
    (hash-table-keys table)))

(define (get-moves table offsets)
  (let ((acc (make-hash-table)) (moves (get-all-moves table offsets)))
    (for-each
      (lambda (move)
        (apply
          (lambda (_ dest)
            (hash-table-update!/default acc dest add1 0))
          move))
      moves)
    (filter
      (lambda (move)
        (apply
          (lambda (_ dest)
            (= (hash-table-ref acc dest) 1))
          move))
      moves)))

(define (run! table offsets)
  (let ((moves (get-moves table offsets)))
    (if (null? moves)
      #f
      (for-each
        (lambda (move)
          (apply
            (lambda (orig dest)
              (hash-table-delete! table orig)
              (hash-table-set! table dest #t))
            move))
        moves))))

(define (convert table)
  (let-values (((xs ys) (unzip2 (hash-table-keys table))))
    (- (* (- (apply max xs) (apply min xs) -1)
          (- (apply max ys) (apply min ys) -1))
       (hash-table-size table))))

(define (solve/1 input)
  (let ((acc (hash-table-copy input)))
    (foldl
      (lambda (offsets _)
        (run! acc offsets) (append (cdr offsets) (list (car offsets))))
      offsets (iota 10))
    (convert acc)))

(define (solve/2 input)
  (let ((acc (hash-table-copy input)))
    (let loop ((i 1) (offsets offsets))
      (if (run! acc offsets)
        (loop (+ i 1) (append (cdr offsets) (list (car offsets))))
        i))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
