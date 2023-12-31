(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (srfi 1))

(define-constant _a (char->integer #\a))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (string->numeric str)
  (map (lambda (i) (- (char->integer i) _a)) (string->list str)))

(define match (map string->numeric (string-split "northpole object storage")))

(define (parse str)
  (let ((lst (string-split str "-[]")))
    (let-values (((name data) (split-at lst (- (length lst) 2))))
      (bind (id checksum) data
        (list (string->number id) (string->numeric checksum) (map string->numeric name))))))

(define (import-input)
  (map parse (read-lines)))

(define (compute lst)
  (let ((mem (make-vector 26 0)) (lst (join lst)))
    (for-each (lambda (i) (vector-set! mem i (+ (vector-ref mem i) 1))) lst)
    (take
      (sort (delete-duplicates lst)
        (lambda (a b)
          (let
            ((m (vector-ref mem a))
             (n (vector-ref mem b)))
            (cond
              ((> m n) #t)
              ((< m n) #f)
              (else (< a b))))))
      5)))

(define (solve/1 input)
  (foldl
    (lambda (acc i)
      (bind (id checksum name) i
        (if (equal? checksum (compute name))
          (+ acc id)
          acc)))
    0 input))

(define (solve/2 input)
  (first
    (find
      (lambda (i)
        (bind (id _ name) i
          (equal? match (map (lambda (i) (map (lambda (i) (modulo (+ i id) 26)) i)) name))))
      input)))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 245102)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 324))))
