(define-record queue buckets minimum)

(define (queue)
  (make-queue (make-hash-table) -1))

(define (queue-push! queue priority data)
  (let ((buckets (queue-buckets queue)))
    (hash-table-set! buckets priority
      (cons data (hash-table-ref/default buckets priority '()))))
  (let ((minimum (queue-minimum queue)))
    ;; set minimum when necessary
    (when (or (= minimum -1) (> minimum priority))
      (queue-minimum-set! queue priority))))

(define (queue-find-minimum queue)
  (let ((priorities (hash-table-keys (queue-buckets queue))))
    (if (null? priorities)
      -1
      (foldl min (car priorities) (cdr priorities)))))

(define (queue-remove! queue priority data)
  (let ((buckets (queue-buckets queue)))
    (let ((datas (delete data (hash-table-ref buckets priority))))
      (if (null? datas)
        (begin
          (hash-table-delete! buckets priority)
          ;; set minimum when necessary
          (when (= (queue-minimum queue) priority)
            (queue-minimum-set! queue (queue-find-minimum queue))))
        (hash-table-set! buckets priority datas)))))

(define (queue-pop! queue)
  (let ((minimum (queue-minimum queue)))
    (if (= minimum -1)
      '()
      (let ((data (car (hash-table-ref (queue-buckets queue) minimum))))
        (queue-remove! queue minimum data)
        (list minimum data)))))
