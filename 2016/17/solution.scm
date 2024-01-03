(import
  (chicken io)
  (euler)
  (srfi 1)
  (simple-md5))

(define-constant offsets
  '((-1  0)
    ( 1  0)
    ( 0 -1)
    ( 0  1)))

(define rooms
  (list->array
    (map string->list
      '("S..."
        "...."
        "...."
        "...V"))))

(define (import-input)
  (read-line))

(define (lookup array target)
  (find
    (lambda (i)
      (char=? (array-ref array i) target))
    (array-indexes array)))

(define (next input array cost coord)
  (filter-map
    (lambda (direction offset data)
      (if (and (char<=? #\b data)
               (char<=? data #\f))
        (let ((coord (map + coord offset)))
          (if (array-exists? array coord)
            (list (string-append cost direction) coord)
            #f))
        #f))
    '("U" "D" "L" "R") offsets (string->list (string->md5sum (string-append input cost)))))

(define (path input array S V proc)
  (let loop ((cost "") (coord S))
    (if (equal? coord V)
      cost
      (let ((lst (filter-map
                   (lambda (i)
                     (apply loop i))
                   (next input array cost coord))))
        (if (null? lst) #f
          (extremum lst string-length proc))))))

(define (solve/1 input)
  (path input rooms
    (lookup rooms #\S)
    (lookup rooms #\V)
    <))

(define (solve/2 input)
  (string-length
    (path input rooms
      (lookup rooms #\S)
      (lookup rooms #\V)
      >)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (string=? part/1 "RDDRLDRURD"))
  (let ((part/2 (solve/2 input)))
    (print part/2) (= part/2 448)))
