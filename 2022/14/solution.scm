(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (euler))

(define (draw-line! array current target)
  (let ((offset (map signum (map - target current))))
    (let loop ((current current))
      (array-set! array current #t)
      (if (equal? current target)
        current
        (loop (map + current offset))))))

(define (create-cave lst)
  (let ((acc (list->array (make-list #e1e3 (make-list #e3e2 #f)))))
    (for-each
      (lambda (report)
        (foldl
          (lambda (current target)
            (draw-line! acc current target))
          (car report) (cdr report)))
      lst)
    acc))

(define (parse-report str)
  (map
    (lambda (_)
      (map string->number (string-split _ ",")))
    (string-split str "-> ")))

(define (import-input)
  (create-cave (map parse-report (read-lines))))

(define (test-offset array current offset)
  (let ((next (map + current offset)))
    (if (array-ref array next)
      #f
      next)))

(define (drop-sand! array bottom)
  (let loop ((current '(500 0)))
    (cond
      ((or (array-ref array current) (> (second current) bottom)) #f)
      ((test-offset array current '( 0 1)) => loop)
      ((test-offset array current '(-1 1)) => loop)
      ((test-offset array current '( 1 1)) => loop)
      (else (array-set! array current #t) #t))))

(define (find-bottom array)
  (let ((_ (filter
             (lambda (coord)
               (array-ref array coord))
             (array-indexes array))))
    (apply max (map second _))))

(define (solve input)
  (let* ((acc (array-copy input)) (bottom (find-bottom acc)))
    (let loop ((i 0))
      (if (drop-sand! acc bottom)
        (loop (+ i 1))
        i))))

(define (draw-floor! array)
  (let ((_ (+ (find-bottom array) 2)))
    (draw-line! array
      (list 0   _)
      (list 999 _))))

(let ((input (import-input)))
  (print (solve input))
  (draw-floor! input)
  (print (solve input)))
