(import (chicken io)
        (chicken process-context)
        (chicken string)
        (srfi 1))

(define (import-input path)
  (call-with-input-file path
    (lambda (file)
      (map
        (lambda (str)
          (string-split str ": -"))
        (read-lines file)))))

(define (is-valid-1? input)
  (let ((lower    (string->number (car  input)))
        (higher   (string->number (cadr input)))
        (letter   (caddr  input))
        (password (cadddr input)))
    (let ((cnt (length (filter
                         (lambda (char)
                           (string=? letter (string char)))
                         (string->list password)))))
      (not (or (< cnt lower)
               (> cnt higher))))))

(define (is-valid-2? input)
  (let ((index-1 (sub1 (string->number (car  input))))
        (index-2 (sub1 (string->number (cadr input))))
        (letter   (caddr  input))
        (password (cadddr input)))
    (letrec* ((helper (lambda (index)
                        (string=? letter (string (string-ref password index)))))
              (res (not (equal? (helper index-1)
                                (helper index-2)))))
      res)))

(define (solve proc lst)
  (display (length (filter proc lst)))
  (newline))

(let ((args (command-line-arguments)))
  (let ((lst (import-input (car args))))
    (solve is-valid-1? lst)
    (solve is-valid-2? lst)))
