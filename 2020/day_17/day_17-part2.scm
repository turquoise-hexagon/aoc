(import (chicken io)
        (chicken process-context)
        (matchable)
        (srfi 1)
        (srfi 69))

(define-record point w h d n)

(define-record world
               content
               h-start h-end
               w-start w-end
               d-start d-end
               n-start n-end)

(define (import-input path)
  (let ((lst (map string->list (read-lines (open-input-file path)))))
    (let ((w (length lst)) (h (length (list-ref lst 0))) (content (make-hash-table)))
      (do ((i 0 (+ i 1))) ((= i h))
        (do ((j 0 (+ j 1))) ((= j w))
          (let ((point (make-point i j 0 0)))
            (hash-table-set! content point (list-ref (list-ref lst i) j)))))
      (make-world content 0 h 0 w 0 1 0 1))))

(define (count-active input)
  (fold
    (lambda (a acc)
      (if (char=? a #\#)
          (+ acc 1)
          acc))
    0 (hash-table-values (world-content input))))

(define (count-neighbors input i j k l)
  (let ((content (world-content input)))
    (set! cnt 0)
    (do ((a -1 (+ a 1))) ((= a 2))
      (do ((b -1 (+ b 1))) ((= b 2))
        (do ((c -1 (+ c 1))) ((= c 2))
          (do ((d -1 (+ d 1))) ((= d 2))
            (when (or (not (= a 0))
                      (not (= b 0))
                      (not (= c 0))
                      (not (= d 0)))
              (let ((point (make-point (+ i a) (+ j b) (+ k c) (+ l d))))
                (when (char=? #\# (hash-table-ref/default content point #\.))
                  (set! cnt (+ cnt 1)))))))))
    cnt))

(define (iterate-world input)
  (match input
    (($ world content h-start h-end w-start w-end d-start d-end n-start n-end)
     (let ((copy-content (hash-table-copy content))
           (copy-h-start (- h-start 1)) (copy-h-end (+ h-end 1))
           (copy-w-start (- w-start 1)) (copy-w-end (+ w-end 1))
           (copy-d-start (- d-start 1)) (copy-d-end (+ d-end 1))
           (copy-n-start (- n-start 1)) (copy-n-end (+ n-end 1)))
       (do ((i copy-h-start (+ i 1))) ((= i copy-h-end))
         (do ((j copy-w-start (+ j 1))) ((= j copy-w-end))
           (do ((k copy-d-start (+ k 1))) ((= k copy-d-end))
             (do ((l copy-n-start (+ l 1))) ((= l copy-n-end))
               (let ((cnt (count-neighbors input i j k l)) (point (make-point i j k l)))
                 (case cnt
                   ((2))
                   ((3)  (hash-table-set! copy-content point #\#))
                   (else (hash-table-set! copy-content point #\.))))))))
       (make-world copy-content
                   copy-h-start copy-h-end
                   copy-w-start copy-w-end
                   copy-d-start copy-d-end
                   copy-n-start copy-n-end)))))

(define (solve/2 input n)
  (let solve/2/h ((n n) (input input))
    (if (= n 0)
        (print (count-active input))
        (solve/2/h (- n 1) (iterate-world input)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/2 input 6)))
