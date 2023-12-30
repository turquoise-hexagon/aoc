(import
  (chicken io)
  (euler)
  (srfi 1))

(define-constant _a (char->integer #\a))

(define-constant _i (- (char->integer #\i) _a))
(define-constant _l (- (char->integer #\l) _a))
(define-constant _o (- (char->integer #\o) _a))

(define (input->number n)
  (list->number
    (map
      (lambda (i)
        (- (char->integer i) _a))
      (string->list n))
    26))

(define (number->input n)
  (list->string
    (map
      (lambda (i)
        (integer->char (+ i _a)))
      (number->list n 26))))

(define (import-input)
  (read-line))

(define (valid? n)
  (let ((lst (number->list n 26)))
    (and (not (any
                (lambda (i)
                  (or (= i _i)
                      (= i _l)
                      (= i _o)))
                lst))
         (any
           (lambda (a b c)
             (= (- a 0)
                (- b 1)
                (- c 2)))
           lst (cdr lst) (cddr lst))
         (= (count
              (lambda (i)
                (= (car i) 2))
              (run-length lst))
            2))))

(define (solve input)
  (do ((i (+ (input->number input) 1) (+ i 1)))
    ((valid? i) (number->input i))))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (string=? part/1 "hepxxyzz"))
    (let ((part/2 (solve part/1)))
      (print part/2) (assert (string=? part/2 "heqaabcc)))))
