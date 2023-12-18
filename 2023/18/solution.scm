(import
  (chicken io)
  (chicken string))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (parse/1 lst)
  (bind (dir distance _) lst
    (list
      (string->symbol dir)
      (string->number distance))))

(define (parse/2 lst)
  (bind (_ _ color) lst
    (bind (distance dir) (string-chop (substring color 1) 5)
      (list
        (string->number dir 16)
        (string->number distance 16)))))

(define (import-input)
  (map
    (lambda (i)
      (string-split i " ()"))
    (read-lines)))

(define (offset dir)
  (case dir
    ((0 R) '( 0  1))
    ((1 D) '( 1  0))
    ((2 L) '( 0 -1))
    ((3 U) '(-1  0))))

(define (run lst)
  (let loop ((lst lst) (coord '(0 0)) (peri 0) (points '()))
    (if (null? lst)
      (list peri (cons '(0 0) points))
      (bind (dir dist) (car lst)
        (loop (cdr lst) (map + coord (map * (offset dir) (list dist dist))) (+ peri dist) (cons coord points))))))

(define (compute peri points)
  (+ (quotient (+ (apply +
                    (map
                      (lambda (a b)
                        (apply * (map (lambda (a b c) (a b c)) (list - +) a b)))
                      points (cdr points)))
                  peri)
               2)
     1))

(define (solve input)
  (apply compute (run input)))

(let ((input (import-input)))
  (let ((part/1 (solve (map parse/1 input))))
    (print part/1) (assert (= part/1 40714)))
  (let ((part/2 (solve (map parse/2 input))))
    (print part/2) (assert (= part/2 129849166997110))))
