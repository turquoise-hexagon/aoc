(import
  (chicken io)
  (srfi 1)
  (srfi 152))

(define GLOBAL-VERSION 0)

(define operators
  `((0 . ,(lambda (_)     (apply   + _)))
    (1 . ,(lambda (_)     (apply   * _)))
    (2 . ,(lambda (_)     (apply min _)))
    (3 . ,(lambda (_)     (apply max _)))
    (5 . ,(lambda (_) (if (apply   > _) 1 0)))
    (6 . ,(lambda (_) (if (apply   < _) 1 0)))
    (7 . ,(lambda (_) (if (apply   = _) 1 0)))))

(define (hexadecimal->binary str)
  (let ((str (number->string (string->number str 16) 2)))
    (string-pad str 4 #\0)))

(define (binary-list->decimal lst)
  (string->number (string-concatenate lst) 2))

(define (import-input)
  (parse
    (string-segment
      (string-concatenate
        (map hexadecimal->binary
          (string-segment
            (read-line)
            1)))
      1)))

(define (parse-header lst)
  (receive (version lst) (split-at lst 3)
    (receive (type-id lst) (split-at lst 3)
      (values version type-id lst))))

(define (parse-length lst len)
  (receive (value lst) (split-at lst len)
    (let ((value (binary-list->decimal value)))
      (values value lst))))

(define (parse-by-length lst)
  (receive (len lst) (parse-length lst 15)
    (receive (packets lst) (split-at lst len)
      (values
        (let loop ((lst packets) (acc '()))
          (if (null? lst)
            (flatten (reverse acc))
            (receive (value lst) (apply values (parse lst))
              (loop lst (cons value acc)))))
        lst))))

(define (parse-by-count lst)
  (receive (len lst) (parse-length lst 11)
    (let loop ((lst lst) (cnt len) (acc '()))
      (if (= cnt 0)
        (values (flatten (reverse acc)) lst)
        (receive (value lst) (apply values (parse lst))
          (loop lst (- cnt 1) (cons value acc)))))))

(define (parse-literal lst)
  (let loop ((lst lst) (acc '()))
    (receive (temp lst) (split-at lst 5)
      (receive (id group) (car+cdr temp)
        (let ((acc (cons group acc)))
          (if (string=? id "1")
            (loop lst acc)
            (let ((value (binary-list->decimal (flatten (reverse acc)))))
              (list value lst))))))))

(define (parse-operator lst type-id)
  (receive (len-id lst) (car+cdr lst)
    (receive (value lst)
      ((if (string=? len-id "0")
         parse-by-length
         parse-by-count)
       lst)
      (list ((cdr (assoc type-id operators)) value) lst))))

(define (parse lst)
  (receive (version type-id lst) (parse-header lst)
    (let ((version (binary-list->decimal version))
          (type-id (binary-list->decimal type-id)))
      (set! GLOBAL-VERSION (+ GLOBAL-VERSION version))
      (if (= type-id 4)
        (parse-literal  lst)
        (parse-operator lst type-id)))))

(let ((input (import-input)))
  (print GLOBAL-VERSION)
  (print (first input)))
