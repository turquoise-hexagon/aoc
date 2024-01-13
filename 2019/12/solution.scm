(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69)
  (only (srfi 133) vector-copy))

(define (import-input)
  (list->vector
    (map
      (lambda (i)
        (list (map string->number (string-split i "<>xyz=, ")) '(0 0 0)))
      (read-lines))))

(define (gravity! data)
  (let ((limit (vector-length data)))
    (do ((a 0 (+ a 1))) ((= a limit))
      (do ((b (+ a 1) (+ b 1))) ((= b limit))
        (apply
          (lambda (coord/a velocities/a coord/b velocities/b)
            (vector-set! data a (list coord/a (map + velocities/a (map signum (map - coord/b coord/a)))))
            (vector-set! data b (list coord/b (map + velocities/b (map signum (map - coord/a coord/b))))))
          (append
            (vector-ref data a)
            (vector-ref data b)))))))

(define (velocity! data)
  (let ((limit (vector-length data)))
    (do ((i 0 (+ i 1))) ((= i limit))
      (apply
        (lambda (coord velocities)
          (vector-set! data i (list (map + coord velocities) velocities)))
        (vector-ref data i)))))

(define (total-energy data)
  (let ((limit (vector-length data)))
    (do ((i 0 (+ i 1))
         (acc 0 (apply
                  (lambda (coord velocities)
                    (+ (* (apply + (map abs coord))
                          (apply + (map abs velocities)))
                       acc))
                  (vector-ref data i))))
      ((= i limit) acc))))

(define (iterate! data)
  (gravity!  data)
  (velocity! data))

(define (id state)
  (string-intersperse (map number->string state)))

(define (states data)
  (let ((limit (vector-length data)))
    (do ((i 0 (+ i 1))
         (acc '() (let ((moon (vector-ref data i)))
                    (cons* (car  moon)
                           (cadr moon)
                           acc))))
      ((= i limit) (map id (apply zip acc))))))

(define (solve/1 input steps)
  (let ((data (vector-copy input)))
    (do ((i 0 (+ i 1)))
      ((= i steps) (total-energy data))
      (iterate! data))))

(define (solve/2 input)
  (let ((data (vector-copy input))
        (cache/x (make-hash-table))
        (cache/y (make-hash-table))
        (cache/z (make-hash-table)))
    (apply
      (lambda (state/x state/y state/z)
        (hash-table-set! cache/x state/x #t)
        (hash-table-set! cache/y state/y #t)
        (hash-table-set! cache/z state/z #t))
      (states data))
    (let loop ((i 1) (done/x #f) (done/y #f) (done/z #f))
      (iterate! data)
      (if (and done/x
               done/y
               done/z)
        (begin
          (lcm done/x
              done/y
              done/z))
        (apply
          (lambda (state/x state/y state/z)
            (loop (+ i 1)
              (if done/x done/x (if (hash-table-exists? cache/x state/x) i #f))
              (if done/y done/y (if (hash-table-exists? cache/y state/y) i #f))
              (if done/z done/z (if (hash-table-exists? cache/z state/z) i #f))))
          (states data))))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input 1000)))
    (print part/1) (assert (= part/1 12644)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 290314621566528))))
