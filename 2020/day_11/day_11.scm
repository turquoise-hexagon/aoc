(import (chicken io)
        (chicken process-context))

(define (import-input path)
  (list->vector (map list->vector (map string->list (read-lines (open-input-file path))))))

(define (copy-world input h w)
  (let ((output (make-vector h)))
    (do ((i 0 (+ i 1))) ((= i h))
      (vector-set! output i (make-vector w))
      (do ((j 0 (+ j 1))) ((= j w))
        (vector-set! (vector-ref output i) j (vector-ref (vector-ref input i) j))))
    output))

(define (count-occupied input h w)
  (set! cnt 0)
  (do ((i 0 (+ i 1))) ((= i h))
    (do ((j 0 (+ j 1))) ((= j w))
      (case (vector-ref (vector-ref input i) j)
        ((#\#) (set! cnt (+ cnt 1))))))
  cnt)

(define (count-neighbors/1 input i j h w)
  (set! cnt 0)
  (do ((a -1 (+ a 1))) ((= a 2))
    (do ((b -1 (+ b 1))) ((= b 2))
      (when (and (or (not (= a 0))
                     (not (= b 0)))
                 (< -1 (+ i a) h)
                 (< -1 (+ j b) w))
        (case (vector-ref (vector-ref input (+ i a)) (+ j b))
          ((#\#) (set! cnt (+ cnt 1)))))))
  cnt)

(define (count-neighbors/2 input i j h w)
  (set! cnt 0)
  (do ((a -1 (+ a 1))) ((= a 2))
    (do ((b -1 (+ b 1))) ((= b 2))
      (when (or (not (= a 0))
                (not (= b 0)))
        (set! cnt (call/cc
                    (lambda (return)
                      (do ((x (+ i a) (+ x a))
                           (y (+ j b) (+ y b)))
                        ((not (and (< -1 x h)
                                   (< -1 y w)))
                         (return cnt))
                        (case (vector-ref (vector-ref input x) y)
                          ((#\#) (return (+ cnt 1)))
                          ((#\L) (return cnt))))))))))
  cnt)

(define (iterate-world proc setting input output h w)
  (do ((i 0 (+ i 1))) ((= i h))
    (do ((j 0 (+ j 1))) ((= j w))
      (let ((cnt (proc input i j h w)))
        (vector-set! (vector-ref output i) j (vector-ref (vector-ref input i) j))
        (case (vector-ref (vector-ref input i) j)
          ((#\#) (when (> cnt setting) (vector-set! (vector-ref output i) j #\L)))
          ((#\L) (when (= cnt       0) (vector-set! (vector-ref output i) j #\#))))))))

(define (solve proc setting input h w)
  (let solve/h ((a (copy-world input h w))
                (b (copy-world input h w)))
    (iterate-world proc setting a b h w)
    (if (equal? a b)
        (print (count-occupied a h w))
        (solve/h b a))))

(let ((path (car (command-line-arguments))))
  (let* ((input (import-input path))
         (h (vector-length input)) (w (vector-length (vector-ref input 0))))
    (solve count-neighbors/1 3 input h w)
    (solve count-neighbors/2 4 input h w)))
