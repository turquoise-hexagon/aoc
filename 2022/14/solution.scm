(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (draw-line! table current target)
  (let ((offset (map signum (map - target current))))
    (let loop ((current current))
      (hash-table-set! table current #t)
      (if (equal? current target)
        current
        (loop (map + current offset))))))

(define (create-cave lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (report)
        (foldl
          (lambda (current target)
            (draw-line! acc current target))
          (car report) (cdr report)))
      lst)
    acc))

(define (import-input)
  (create-cave
    (map
      (lambda (_)
        (chop (map string->number (string-split _ "->, ")) 2))
      (read-lines))))

(define (find-bottom table)
  (apply max (map cadr (hash-table-keys table))))

(define (test-offsets table coord)
  (let loop ((offsets '((0 1) (-1 1) (1 1))))
    (if (null? offsets)
      #f
      (let ((next (map + coord (car offsets))))
        (if (hash-table-exists? table next)
          (loop (cdr offsets))
          next)))))

(define (make-drop-sand!)
  (set! acc '(_ (500 0))) ;; init cache
  (lambda (table bottom)
    (set! acc (cdr acc)) ;; trim cache 
    (let loop ()
      (if (or (null? acc) (> (second (car acc)) bottom))
        #f
        (cond
          ((test-offsets table (car acc)) =>
           (lambda (next)
             (set! acc (cons next acc))
             (loop)))
          (else
           (hash-table-set! table (car acc) #t)
           #t))))))

(define (solve input)
  (let* ((acc (hash-table-copy input)) (bottom (find-bottom acc)) (drop-sand! (make-drop-sand!)))
    (let loop ((i 0))
      (if (drop-sand! acc bottom)
        (loop (+ i 1))
        i))))

(define (draw-floor! table)
  (let ((_ (+ (find-bottom table) 2)))
    (draw-line! table
      (list -1000 _)
      (list  1000 _))))

(let ((input (import-input)))
  (print (solve input))
  (draw-floor! input)
  (print (solve input)))
