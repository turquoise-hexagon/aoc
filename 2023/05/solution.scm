(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define-constant inf #e1e32)

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse str)
  (map string->number (irregex-extract "[0-9]+" str)))

(define (import-input)
  (bind (seeds . maps) (irregex-split "\n\n" (read-string))
    (values (map (lambda (i) (chop (parse i) 3)) maps) (parse seeds))))

(define (process seeds maps)
  (foldl
    (lambda (acc seed)
      (bind (a b) seed
        (bind (d s r) (let ((_ (find (lambda (i) (bind (d s r) i (<= s a (+ s r -1)))) maps))) (if _ _ (list inf inf inf)))
          (let ((m (+ a b))
                (n (+ s r)))
            (cons (list (+ d (- a s)) (- (min m n) a))
              (if (> m n)
                (append acc (process (list (list n (- m n))) maps))
                acc))))))
    '() seeds))

(define (solve maps seeds)
  (apply min (map first (foldl process seeds maps))))

(let-values (((maps seeds) (import-input)))
  (let ((part/1 (solve maps (map (lambda (i) (list i 1)) seeds))))
    (print part/1) (assert (= part/1 51752125)))
  (let ((part/2 (solve maps (chop seeds 2))))
    (print part/2) (assert (= part/2 12634632))))
