(import
  (chicken fixnum)
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define-syntax bind
  (syntax-rules ()
    ((_ pattern data expression expression* ...)
     (apply (lambda pattern expression expression* ...) data))))

(define (parse-header str)
  (bind (_ . data) (irregex-split " " str)
    (map string->number data)))

(define (parse-map str)
  (bind (_ . data) (irregex-split "\n" str)
    (map
      (lambda (i)
        (map string->number (irregex-split " " i)))
      data)))

(define (import-input)
  (bind (header . maps) (irregex-split "\n\n" (read-string))
    (values (parse-header header) (map parse-map maps))))

(define (process seed maps)
  (foldl
    (lambda (seed mappings)
      (let loop ((mappings mappings))
        (if (null? mappings)
          seed
          (bind (destination source range-length) (car mappings)
            (if (and (fx<= source seed) (fx<= seed (fx- (fx+ source range-length) 1)))
              (fx+ destination (fx- seed source))
              (loop (cdr mappings)))))))
    seed maps))

(define (transform maps)
  (map
    (lambda (mappings)
      (map
        (lambda (mapping)
          (let-values (((a b) (split-at mapping 2)))
            (append (reverse a) b)))
        (reverse mappings)))
    (reverse maps)))

(define (solve/1 header maps)
  (foldl
    (lambda (acc i)
      (fxmin acc (process i maps)))
    most-positive-fixnum header))

(define (solve/2 header maps)
  (let ((header (chop header 2)) (maps (transform maps)))
    (let loop ((i 0))
      (let subloop ((header header))
        (if (null? header)
          (loop (fx+ i 1))
          (let ((seed (process i maps)))
            (bind (source range-length) (car header)
              (if (and (fx<= source seed) (fx<= seed (fx- (fx+ source range-length) 1)))
                i
                (subloop (cdr header))))))))))

(let-values (((headers maps) (import-input)))
  (let ((part/1 (solve/1 headers maps)))
    (print part/1) (assert (= part/1 51752125)))
  (let ((part/2 (solve/2 headers maps)))
    (print part/2) (assert (= part/2 12634632))))
