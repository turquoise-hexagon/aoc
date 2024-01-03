(import
  (chicken io)
  (chicken fixnum)
  (euler)
  (srfi 1))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (string->number (read-line)))

(define-inline (_function a b)
  (fx+ (fx+ (fx+ (fx* a a) (fx* 3 a)) (fx+ (fx* 2 (fx* a b)) (fx* b b))) b))

(define-inline (_open? input a b)
  (let loop ((i (fx+ input (_function a b))) (acc 0))
    (if (fx= i 0)
      (fx= acc 0)
      (loop (fxshr i 1) (fxxor acc (fxand i 1))))))

(define (open? input coord)
  (apply
    (lambda (a b)
      (and (fx>= a 0)
           (fx>= b 0)
           (_open? input a b)))
    coord))

(define (compare? a b)
  (fx< (car a)
       (car b)))

(define (next input cost coord)
  (let ((_ (fx+ cost 1)))
    (map
      (lambda (i)
        (list _ i))
      (filter
        (lambda (i)
          (open? input i))
        (map
          (lambda (i)
            (map fx+ coord i))
          offsets)))))

(define (path input end flag)
  (let ((acc (make-array '(50 50) #f)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) '(0 (1 1)))))
      (if (priority-queue-empty? queue)
        acc
        (apply
          (lambda (cost coord)
            (if (equal? coord end)
              cost
              (loop
                (foldl
                  (lambda (queue i)
                    (apply
                      (lambda (cost coord)
                        (if (and (or (not (array-ref acc coord)) (fx< cost (array-ref acc coord))) (if flag (fx<= cost 50) #t))
                          (begin
                            (array-set! acc coord cost)
                            (priority-queue-insert queue i))
                          queue))
                      i))
                  (priority-queue-rest queue) (next input cost coord)))))
          (priority-queue-first queue))))))

(define (solve/1 input)
  (path input '(31 39) #f))

(define (solve/2 input)
  (let ((acc (path input '(50 50) #t)))
    (count
      (lambda (i)
        (array-ref acc i))
      (array-indexes acc))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 96)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 141))))
