(import
  (chicken io)
  (chicken string))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (parse/1 lst)
  (bind (dir dist _) lst
    (list
      (string->symbol dir)
      (string->number dist))))

(define (parse/2 lst)
  (bind (_ _ hex) lst
    (bind (dist dir) (string-chop (substring hex 1) 5)
      (list
        (string->number dir 16)
        (string->number dist 16)))))

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
  (let loop ((lst lst) (pt '(0 0)) (peri 0) (pts '()))
    (if (null? lst)
      (list peri pts)
      (bind (dir dist) (car lst)
        (loop (cdr lst)
          (map + pt (map * (offset dir) (list dist dist)))
          (+ peri dist)
          (cons pt pts))))))

(define (solve input)
  (bind (peri pts) (run input)
    (+ (quotient
         (+ (apply +
              (map
                (lambda (a b)
                  (apply * (map (cut <> <> <>) (list - +) a b)))
                pts (cdr pts)))
            peri)
         2)
       1)))

(let ((input (import-input)))
  (let ((part/1 (solve (map parse/1 input))))
    (print part/1) (assert (= part/1 40714)))
  (let ((part/2 (solve (map parse/2 input))))
    (print part/2) (assert (= part/2 129849166997110))))
