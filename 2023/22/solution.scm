(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ",~")))
    (read-lines)))

(define (coordinates brick)
  (apply
    (lambda (a b)
      (apply product
        (map
          (lambda (a b)
            (let ((_ (max a 0)))
              (if (= a b) (list _) (range _ b))))
          a b)))
    (chop brick 3)))

(define (down brick)
  (apply
    (lambda (a b c d e f)
      (list a b (- c 1) d e (- f 1)))
    brick))

(define (gravity! mem bricks)
  (foldl
    (lambda (acc brick)
      (let loop ((brick brick))
        (let ((next (down brick)))
          (if (or (= (list-ref brick 2) 0)
                  (any
                    (lambda (coord)
                      (array-ref mem coord))
                    (coordinates next)))
            (begin
              (for-each
                (lambda (coord)
                  (array-set! mem coord brick))
                (coordinates brick))
              (cons brick acc))
            (loop next)))))
    '() (sort bricks
          (lambda (a b)
            (< (list-ref a 2)
               (list-ref b 2))))))

(define (gravity bricks)
  (let* ((mem (make-array '(10 10 300) #f)) (fell (gravity! mem bricks)))
    (values mem fell)))

(define-inline (id brick)
  (string-intersperse (map number->string brick)))

(define (process mem fell)
  (let
    ((above (make-hash-table #:initial '()))
     (below (make-hash-table #:initial '())))
    (for-each
      (lambda (brick)
        (let ((tmp (coordinates brick)))
          (for-each
            (lambda (coord)
              (when (and (array-ref mem coord) (not (member coord tmp)))
                (let ((value (array-ref mem coord)))
                  (hash-table-update! above (id value) (cut cons brick <>))
                  (hash-table-update! below (id brick) (cut cons value <>)))))
            (coordinates (down brick)))))
      fell)
    (values above below)))

(define (desintegrate above below brick)
  (let ((acc (make-hash-table)))
    (let loop ((brick brick))
      (let ((tmp (id brick)))
        (unless (hash-table-exists? acc tmp)
          (hash-table-set! acc tmp #t)
          (for-each
            (lambda (brick)
              (when (every
                      (lambda (brick)
                        (hash-table-exists? acc (id brick)))
                      (hash-table-ref below (id brick)))
                (loop brick)))
            (hash-table-ref/default above tmp '())))))
    (hash-table-size acc)))

(define (solve bricks)
  (let-values (((mem fell) (gravity bricks)))
    (let-values (((above below) (process mem fell)))
      (let loop ((bricks fell) (acc/1 0) (acc/2 0))
        (if (null? bricks)
          (list acc/1 acc/2)
          (let ((_ (desintegrate above below (car bricks))))
            (loop (cdr bricks)
              (if (= _ 1)
                (+ acc/1 1)
                acc/1)
              (+ acc/2 _ -1))))))))

(let ((parts (solve (import-input))))
  (for-each print parts)
  (assert (equal? parts '(432 63166))))
