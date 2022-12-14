(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define (draw! table current target)
  (let ((offset (map signum (map - target current))))
    (let loop ((current current))
      (hash-table-set! table current #t)
      (if (equal? current target)
        current
        (loop (map + current offset))))))

(define (draw-floor! table)
  (let ((bound (+ (apply max (map cadr (hash-table-keys table))) 2)))
    (draw! table
      (list 0    bound)
      (list 1000 bound))))

(define (parse-report str)
  (map
    (lambda (_)
      (map string->number (string-split _ ",")))
    (string-split str "-> ")))

(define (cave lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (report)
        (foldl
          (lambda (current target)
            (draw! acc current target))
          (car report) (cdr report)))
      lst)
    acc))

(define (import-input)
  (cave (map parse-report (read-lines))))

(define (test-offset table current offset)
  (let ((next (map + current offset)))
    (if (hash-table-exists? table next)
      #f
      next)))

(define (drop-sand! table bound)
  (let loop ((current '(500 0)))
    (cond
      ((or (hash-table-exists? table current) (> (cadr current) bound))
       #f)
      ((test-offset table current '( 0 1)) => loop)
      ((test-offset table current '(-1 1)) => loop)
      ((test-offset table current '( 1 1)) => loop)
      (else
       (hash-table-set! table current #t)
       #t))))

(define (solve input)
  (let ((bound (apply max (map cadr (hash-table-keys input)))))
    (let loop ((i 0))
      (if (drop-sand! input bound)
        (loop (+ i 1))
        i))))

(let* ((input/1 (import-input)) (input/2 (hash-table-copy input/1)))
  (draw-floor! input/2)
  (print (solve input/1))
  (print (solve input/2)))
