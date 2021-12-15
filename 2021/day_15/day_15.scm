(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(include-relative "grid.scm")
(include-relative "queue.scm")

(define (import-input)
  (list->grid
    (map (cut map string->number <>)
      (map (cut string-chop <> 1)
        (read-lines)))))

(define (transform/h lst i)
  (map
    (lambda (n)
      (+ (modulo (+ n i -1) 9) 1))
    lst))

(define (transform grid n)
  (let* ((lst (grid->list grid))
         (lst (join (map
                      (lambda (i)
                        (map (cut transform/h <> i) lst))
                      (iota n))))
         (lst (map
                (lambda (lst)
                  (join (map (cut transform/h lst <>) (iota n))))
                lst)))
    (list->grid lst)))

(define (neighbors grid coord)
  (filter (cut grid-exists? grid <>)
    (map (cut map + <> coord) '((1 0) (0 1) (-1 0) (0 -1)))))

(define (solve grid from to)
  (let ((acc (make-hash-table)) (queue (queue 10000)))
    (queue-push! queue 0 from)
    (let loop ((current (queue-pop! queue)))
      (if (null? current) (hash-table-ref acc to)
        (receive (distance coord) (apply values current)
          (for-each
            (lambda (neighbor)
              (let ((distance (+ distance (grid-ref grid neighbor))))
                (if (hash-table-exists? acc neighbor)
                  (when (> (hash-table-ref acc neighbor) distance)
                    (hash-table-set! acc neighbor distance))
                  (begin
                    (hash-table-set! acc neighbor distance)
                    (queue-push! queue distance neighbor)))))
            (neighbors grid coord))
          (loop (queue-pop! queue)))))))

(let* ((input/1 (import-input)) (input/2 (transform input/1 5)))
  (print (solve input/1 '(0 0) (grid-end input/1)))
  (print (solve input/2 '(0 0) (grid-end input/2))))
