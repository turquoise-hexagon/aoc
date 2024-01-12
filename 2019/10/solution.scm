(import
  (chicken io)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69))

(define (detected station asteroids)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (asteroid)
        (unless (equal? asteroid station)
          (let* ((diff (map - asteroid station))
                 (temp (make-list 2 (apply gcd diff)))
                 (diff (map quotient diff temp)))
            (hash-table-set! acc diff #t))))
      asteroids)
    (hash-table-keys acc)))

(define (asteroids array)
  (filter
    (lambda (coord)
      (char=? (array-ref array coord) #\#))
    (array-indexes array)))

(define (station asteroids)
  (extremum asteroids
    (lambda (station)
      (length (detected station asteroids)))
    >))

(define (import-input)
  (let* ((asteroids (asteroids (list->array (map string->list (read-lines)))))
         (station (station asteroids))
         (detected (detected station asteroids)))
    (values asteroids station detected)))

(define (destroyed detected)
  (map cdr
    (sort
      (map
        (lambda (diff)
          (cons (apply atan (reverse diff)) diff))
        detected)
      (lambda (a b)
        (> (car a)
           (car b))))))

(define (solve/1 detected)
  (length detected))

(define (solve/2 asteroids station detected)
  (let ((cache (make-hash-table)) (diff (list-ref (destroyed detected) (- 200 1))))
    (for-each
      (lambda (asteroid)
        (hash-table-set! cache asteroid #t))
      asteroids)
    (let loop ((coord station))
      (let ((coord (map + coord diff)))
        (if (hash-table-exists? cache coord)
          (apply + (map * '(1 100) coord))
          (loop coord))))))

(let-values (((asteroids station detected) (import-input)))
  (let ((part/1 (solve/1 detected)))
    (print part/1) (assert (= part/1 230)))
  (let ((part/2 (solve/2 asteroids station detected)))
    (print part/2) (assert (= part/2 1205))))
