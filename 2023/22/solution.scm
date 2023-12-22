(import
  (chicken fixnum)
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-inline (adjust n)
  (if (fx< n 0)
    (fx- -1 (fx+ n n))
    (fx+ n n)))

(define-inline (cantor a b)
  (let*
    ((a (adjust a))
     (b (adjust b))
     (_ (fx+ a b)))
    (fx+ (fx/ (fx* _ (fx+ _ 1)) 2) b)))

(define-inline (id-coord a b c)
  (cantor (cantor a b) c))

(define-inline (id-brick brick)
  (string-intersperse (map number->string brick)))

(define (import-input)
  (map
    (lambda (i)
      (map string->number (string-split i ",~")))
    (read-lines)))

(define (positions brick)
  (apply
    (lambda (a b)
      (apply product
        (map
          (lambda (a b)
            (if (= a b) (list a) (range a b)))
          a b)))
    (chop brick 3)))

(define (down brick)
  (map + brick '(0 0 -1 0 0 -1)))

(define (gravity! mem bricks)
  (foldl
    (lambda (acc brick)
      (let loop ((brick brick))
        (let ((next (down brick)))
          (if (or (any
                    (lambda (coord)
                      (hash-table-exists? mem (apply id-coord coord)))
                    (positions next))
                  (= (list-ref brick 2) 0))
            (begin
              (for-each
                (lambda (coord)
                  (hash-table-set! mem (apply id-coord coord) brick))
                (positions brick))
              (cons brick acc))
            (loop next)))))
    '() (sort bricks
          (lambda (a b)
            (< (list-ref a 2)
               (list-ref b 2))))))

(define (gravity bricks)
  (let* ((mem (make-hash-table)) (fell (gravity! mem bricks)))
    (values mem fell)))

(define (process mem fell)
  (let
    ((above (make-hash-table #:initial '()))
     (below (make-hash-table #:initial '())))
    (for-each
      (lambda (brick)
        (let ((tmp (make-hash-table)))
          (for-each
            (lambda (coord)
              (hash-table-set! tmp (apply id-coord coord) #t))
            (positions brick))
          (for-each
            (lambda (coord)
              (let ((id (apply id-coord coord)))
                (when (and (hash-table-exists? mem id) (not (hash-table-exists? tmp id)))
                  (let ((value (hash-table-ref mem id)))
                    (hash-table-update! above (id-brick value) (lambda (_) (cons brick _)))
                    (hash-table-update! below (id-brick brick) (lambda (_) (cons value _)))))))
            (positions (down brick)))))
      fell)
    (values above below)))

(define (desintegrate above below brick)
  (let ((acc (make-hash-table)))
    (let loop ((brick brick))
      (let ((id (id-brick brick)))
        (unless (hash-table-exists? acc id)
          (hash-table-set! acc id #t)
          (for-each
            (lambda (brick)
              (when (every
                      (lambda (brick)
                        (hash-table-exists? acc (id-brick brick)))
                      (hash-table-ref below (id-brick brick)))
                (loop brick)))
            (hash-table-ref/default above id '())))))
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
