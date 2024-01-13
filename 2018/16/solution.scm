(import
  (chicken io)
  (chicken fixnum)
  (chicken irregex)
  (matchable)
  (srfi 1)
  (srfi 69))

(define-inline (get address)       (vector-ref  mem address))
(define-inline (set address value) (vector-set! mem address value))

(define instructions
  `(("addr" . ,(lambda (mem a b c) (set c (+     (get a) (get b)))))
    ("addi" . ,(lambda (mem a b c) (set c (+     (get a)      b))))
    ("mulr" . ,(lambda (mem a b c) (set c (*     (get a) (get b)))))
    ("muli" . ,(lambda (mem a b c) (set c (*     (get a)      b))))
    ("banr" . ,(lambda (mem a b c) (set c (fxand (get a) (get b)))))
    ("bani" . ,(lambda (mem a b c) (set c (fxand (get a)      b))))
    ("borr" . ,(lambda (mem a b c) (set c (fxior (get a) (get b)))))
    ("bori" . ,(lambda (mem a b c) (set c (fxior (get a)      b))))
    ("setr" . ,(lambda (mem a b c) (set c (get a))))
    ("seti" . ,(lambda (mem a b c) (set c      a)))
    ("gtir" . ,(lambda (mem a b c) (set c (if (>      a  (get b)) 1 0))))
    ("gtri" . ,(lambda (mem a b c) (set c (if (> (get a)      b)  1 0))))
    ("gtrr" . ,(lambda (mem a b c) (set c (if (> (get a) (get b)) 1 0))))
    ("eqir" . ,(lambda (mem a b c) (set c (if (=      a  (get b)) 1 0))))
    ("eqri" . ,(lambda (mem a b c) (set c (if (= (get a)      b)  1 0))))
    ("eqrr" . ,(lambda (mem a b c) (set c (if (= (get a) (get b)) 1 0))))))

(define (parse-samples chunk)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (map string->number (irregex-extract "[0-9]+" i)))
        (irregex-split "\n" i)))
    (irregex-split "\n{2}" chunk)))

(define (parse-program chunk)
  (map
    (lambda (i)
      (map string->number (irregex-extract "[0-9]+" i)))
    (irregex-split "\n" chunk)))

(define (import-input)
  (apply
    (lambda (samples program)
      (values
        (parse-samples samples)
        (parse-program program)))
    (irregex-split "\n{3}" (read-string))))

(define (matching sample instructions)
  (match sample
    ((a (_ . data) b)
     (filter
       (match-lambda
         ((_ . function)
          (let ((mem/a (list->vector a))
                (mem/b (list->vector b)))
            (apply function mem/a data)
            (equal? mem/a mem/b))))
       instructions))))

(define (matched samples)
  (let ((acc (make-vector 16 #f)))
    (let loop ((instructions instructions))
      (if (null? instructions)
        acc
        (let ((mem (make-hash-table)))
          (for-each
            (lambda (sample)
              (match (matching sample instructions)
                ((instruction)
                 (match-let (((_ (code . _) _) sample) ((name . _) instruction))
                   (vector-set! acc code instruction)
                   (hash-table-set! mem name #t)))
                (_ (void))))
            samples)
          (loop (foldr alist-delete instructions (hash-table-keys mem))))))))

(define (solve/1 samples)
  (count
    (lambda (sample)
      (>= (length (matching sample instructions)) 3))
    samples))

(define (solve/2 samples program)
  (let ((matched (matched samples)))
    (let ((mem (make-vector 5 0)))
      (for-each
        (match-lambda
          ((code . data)
           (match (vector-ref matched code)
             ((_ . function)
              (apply function mem data)))))
        program)
      (vector-ref mem 0))))

(let-values (((samples program) (import-input)))
  (let ((part/1 (solve/1 samples)))
    (print part/1) (assert (= part/1 560)))
  (let ((part/2 (solve/2 samples program)))
    (print part/2) (assert (= part/2 622))))
