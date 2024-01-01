(import
  (chicken io)
  (chicken string)
  (simple-md5))

(define-constant zero  (char->integer #\0))
(define-constant match (make-string 5 #\0))

(define (import-input)
  (read-line))

(define (solve input)
  (let
    ((acc/1 (make-vector 8 #f))
     (acc/2 (make-vector 8 #f)))
    (let loop ((i 0) (cnt/1 0) (cnt/2 0))
      (if (and (> cnt/1 7)
               (> cnt/2 7))
        (map list->string (map vector->list (list acc/1 acc/2)))
        (let ((result (string->md5sum (string-append input (number->string i)))))
          (if (substring=? match result)
            (let*
              ((index (- (char->integer (string-ref result 5)) zero))
               (flag/1      (not (> cnt/1 7)))
               (flag/2 (and (not (> index 7)) (not (vector-ref acc/2 index)))))
              (when flag/1 (vector-set! acc/1 cnt/1 (string-ref result 5)))
              (when flag/2 (vector-set! acc/2 index (string-ref result 6)))
              (loop (+ i 1)
                (if flag/1 (+ cnt/1 1) cnt/1)
                (if flag/2 (+ cnt/2 1) cnt/2)))
            (loop (+ i 1) cnt/1 cnt/2)))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (assert (equal? parts '("f77a0e6e" "999828ec"))))
