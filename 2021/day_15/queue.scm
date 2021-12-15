(define-record queue
  buckets
  size
  minimum)

(define (queue size)
  (make-queue (make-vector size '()) size (+ size 1)))

(define (queue-push! queue priority data)
  (let ((buckets (queue-buckets queue)))
    (vector-set! buckets priority
      (cons data (vector-ref buckets priority)))
    ;; update minimum when necessary
    (when (> (queue-minimum queue) priority)
      (queue-minimum-set! queue priority))))


(define (queue-find-minimum queue index)
  (let ((buckets (queue-buckets queue)) (size (queue-size queue)))
    (let loop ((index index))
      (if (= index size)
        (+ size 1)
        (if (null? (vector-ref buckets index))
          (loop (+ index 1))
          index)))))

(define (queue-remove! queue priority data)
  (let ((buckets (queue-buckets queue)))
    (vector-set! buckets priority
      (delete data (vector-ref buckets priority)))
    ;; update minimum when necessary
    (when (= (queue-minimum queue) priority)
      (queue-minimum-set! queue (queue-find-minimum queue priority)))))

(define (queue-pop! queue)
  (let ((buckets (queue-buckets queue)) (minimum (queue-minimum queue)))
    ;; don't return anything when bucket is empty
    (if (> minimum (queue-size queue))
      '()
      (let ((data (car (vector-ref buckets minimum))))
        (queue-remove! queue minimum data)
        (list minimum data)))))
