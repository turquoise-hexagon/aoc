(import
  (chicken io)
  (chicken string)
  (srfi 1))

(define-constant padding-length 200)

(define (parse-init lst)
  (apply
    (lambda (_ _ data)
      data)
    (string-split (car lst) ": ")))

(define (parse-rules lst)
  (map
    (lambda (i)
      (string-split i " =>"))
    lst))

(define (import-input)
  (apply
    (lambda (init rules)
      (values
        (parse-init  init)
        (parse-rules rules)))
    (foldr
      (lambda (i acc)
        (if (string=? i "")
          (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))))

(define (iterate init rules)
  (let ((l (string-length init)))
    (let loop ((i 0) (acc ""))
      (if (= i l)
        acc
        (let ((chunk (substring init
                       (max 0 (- i 2))
                       (min l (+ i 3)))))
          (let subloop ((lst rules))
            (if (null? lst)
              (loop (+ i 1) (string-append acc "."))
              (apply
                (lambda (source dest)
                  (if (string=? chunk source)
                    (loop (+ i 1) (string-append acc dest))
                    (subloop (cdr lst))))
                (car lst)))))))))

(define (solve init rules iterations)
  (let ((padding (apply string-append (make-list padding-length "."))))
    (let loop ((i 0) (acc (string-append padding init padding)))
      (let ((nxt (iterate acc rules)))
        (if (or (= i iterations)
                (string=?
                  (substring acc 0 (+ (* 2 padding-length) 0))
                  (substring nxt 1 (+ (* 2 padding-length) 1))))
          (fold
            (lambda (value index acc)
              (if (string=? value "#")
                (+ acc (+ (- index padding-length) (- iterations i)))
                acc))
            0 (string-chop acc 1) (iota (string-length acc)))
          (loop (+ i 1) (iterate acc rules)))))))

(let-values (((init rules) (import-input)))
  (let ((part/1 (solve init rules 20)))
    (print part/1) (assert (= part/1 1991)))
  (let ((part/2 (solve init rules #e5e10)))
    (print part/2) (assert (= part/2 1100000000511))))
