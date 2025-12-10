(import
  (chicken io)
  (chicken string)
  (euler)
  (euler-syntax)
  (srfi 69))

(define (parse-diagram str)
  (bind (str) (string-split str "[]")
    str))

(define (parse-buttons lst)
  (map
    (lambda (i)
      (map string->number (string-split i "(),")))
    lst))

(define (parse-joltage str)
  (list->vector (map string->number (string-split str "{},"))))

(define (import-input)
  (map
    (lambda (i)
      (bind (diagram . tmp) (string-split i " ")
        (bind (_ . buttons) (reverse tmp)
          (list
            (parse-diagram diagram)
            (parse-buttons buttons)))))
    (read-lines)))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (next cost state buttons)
  (map
    (lambda (lst)
      (let ((acc (string-copy state)))
        (for-each
          (lambda (i)
            (string-set! acc i
              (case (string-ref acc i)
                ((#\.) #\#)
                ((#\#) #\.))))
          lst)
        (list (+ cost 1) acc)))
    buttons))

(define (init diagram)
  (make-string (string-length diagram) #\.))

(define (search diagram buttons)
  (let ((mem (make-hash-table)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 (init diagram)))))
      (if (priority-queue-empty? queue)
        mem
        (bind (cost state) (priority-queue-first queue)
          (if (string=? state diagram)
            cost
            (loop
              (foldl
                (lambda (queue i)
                  (bind (cost state) i
                    (if (> (hash-table-ref/default mem state +inf.0) cost)
                      (begin
                        (hash-table-set! mem state cost)
                        (priority-queue-insert queue i))
                      queue)))
                (priority-queue-rest queue) (next cost state buttons)))))))))

(define (solve input)
  (apply +
    (map
      (lambda (i)
        (bind (diagram buttons _) i
          (search diagram buttons)))
      input)))

(let* ((input (import-input)) (part (solve input)))
  (print part) (assert (= part 522)))
