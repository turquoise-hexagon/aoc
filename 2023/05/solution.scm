(import
  (chicken io)
  (chicken irregex))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse str)
  (map string->number (irregex-extract "[0-9]+" str)))

(define (parse-mappings str)
  (chop (parse str) 3))

(define (import-input)
  (bind (seeds . maps) (irregex-split "\n\n" (read-string))
    (values (parse seeds) (map parse-mappings maps))))

(define (transform/1 seeds)
  (map
    (lambda (i)
      (list i i))
    seeds))

(define (transform/2 seeds)
  (map
    (lambda (i)
      (bind (a b) i
        (list a (+ a b -1))))
    (chop seeds 2)))

(define (process seed maps)
  (bind (a b) seed
    (if (null? maps) a
      (let loop ((mappings (car maps)))
        (if (null? mappings)
          (process seed (cdr maps))
          (bind (d s r) (car mappings)
            (cond
              ((and (<= s a (+ s r -1)) (<= s b (+ s r -1)))
               (process `(,(+ d (- a s)) ,(+ d (- b s))) (cdr maps)))
              ((and (<= s a (+ s r -1)) (< (+ s r) b))
               (min (process `(,(+ s r) ,b) maps)
                    (process `(,(+ d (- a s)) ,(+ d r)) (cdr maps))))
              ((and (< a s) (<= s b (+ s r -1)))
               (min (process `(,a ,(- s 1)) maps)
                    (process `(,d ,(+ d (- b s))) (cdr maps))))
              ((and (< a s) (> b (+ s r)))
               (min (process `(,a ,(- s 1)) maps)
                    (process `(,(+ s r) ,b) maps)
                    (process `(,d ,(+ d r)) (cdr maps))))
              (else (loop (cdr mappings))))))))))

(define (solve seeds maps)
  (apply min
    (map
      (lambda (i)
        (process i maps))
      seeds)))

(let-values (((seeds maps) (import-input)))
  (let ((part/1 (solve (transform/1 seeds) maps)))
    (print part/1) (assert (= part/1 51752125)))
  (let ((part/2 (solve (transform/2 seeds) maps)))
    (print part/2) (assert (= part/2 12634632))))
