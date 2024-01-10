(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (process! array depth origin target coord)
  (let loop ((i coord))
    (if (array-ref array i) (array-ref array i)
      (let*
        ((a (cond
              ((equal? i origin) 0)
              ((equal? i target) 0)
              (else
               (bind (x y) i
                 (cond
                   ((= x 0) (* y 48271))
                   ((= y 0) (* x 16807))
                   (else
                    (bind (_ m _ _ n _)
                      (append
                        (loop (list (- x 1) y))
                        (loop (list x (- y 1))))
                      (* m n))))))))
         (b (modulo (+ a depth) 20183))
         (c (modulo (+ b 0) 3))
         (acc (list a b c)))
        (array-set! array i acc)
        acc))))

(define (generate depth target)
  (let ((acc (make-array '(1000 1000) #f)))
    (for-each
      (lambda (i)
        (process! acc depth '(0 0) target i))
      (array-indexes acc))
    acc))

(define (import-input)
  (bind (depth . target) (filter-map string->number (string-split (read-string) " ,\n"))
    (values (generate depth target) target)))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (next array cost gear coord)
  (filter-map
    (lambda (i)
      (bind (cost+ gear coord+) i
        (let* ((cost (+ cost cost+)) (coord (map + coord coord+)) (acc (list cost gear coord)))
          (if (array-exists? array coord)
            (case (last (array-ref array coord))
              ((0)
               (case gear
                 ((torch) acc)
                 ((climb) acc)
                 (else    #f)))
              ((1)
               (case gear
                 ((empty) acc)
                 ((climb) acc)
                 (else    #f)))
              ((2)
               (case gear
                 ((empty) acc)
                 ((torch) acc)
                 (else    #f))))
            #f))))
    `((1 ,gear (-1  0))
      (1 ,gear ( 0  1))
      (1 ,gear ( 1  0))
      (1 ,gear ( 0 -1))
      (7 empty ( 0  0))
      (7 torch ( 0  0))
      (7 climb ( 0  0)))))

(define (solve/1 array target)
  (apply +
    (map
      (lambda (i)
        (last (array-ref array i)))
      (apply product (map range target)))))

(define (id gear coord)
  (string-append (symbol->string gear) (string-intersperse (map number->string coord))))

(define (solve/2 array target)
  (let ((cache (make-hash-table)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 'torch '(0 0)))))
      (if (priority-queue-empty? queue)
        cache
        (bind (cost gear coord) (priority-queue-first queue)
          (if (and (equal? gear 'torch) (equal? coord target))
            cost
            (loop
              (foldl
                (lambda (queue i)
                  (bind (cost gear coord) i
                    (let ((id (id gear coord)))
                      (if (or (not (hash-table-exists? cache id)) (< cost (hash-table-ref cache id)))
                        (begin
                          (hash-table-set! cache id cost)
                          (priority-queue-insert queue i))
                        queue))))
                (priority-queue-rest queue) (next array cost gear coord)))))))))

(let-values (((array target) (import-input)))
  (let ((part/1 (solve/1 array target)))
    (print part/1) (assert (= part/1 11462)))
  (let ((part/2 (solve/2 array target)))
    (print part/2) (assert (= part/2 1054))))
