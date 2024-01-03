(import
  (chicken io)
  (chicken irregex)
  (euler)
  (srfi 1))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant offsets
  '((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (bind (_ _ . data) (read-lines)
    (map
      (lambda (i)
        (bind (x y size used avail use%)
          (map string->number (irregex-extract "[0-9]+" i))
          (list (list x y) size used avail use%)))
      data)))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (next array cost open goal)
  (filter-map
    (lambda (i)
      (let ((next (map + open i)))
        (if (and (array-exists? array next) (array-ref array next))
          (if (equal? next goal)
            (list (+ cost 1) goal open)
            (list (+ cost 1) next goal))
          #f)))
    offsets))

(define (path array open goal end)
  (let ((cache (make-array (join (make-list 2 (array-dimensions array))) #f)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 open goal))))
      (if (priority-queue-empty? queue)
        cache
        (bind (cost open goal) (priority-queue-first queue)
          (if (equal? goal end)
            cost
            (loop
              (foldl
                (lambda (queue i)
                  (bind (cost open goal) i
                    (let ((id (append open goal)))
                      (if (or (not (array-ref cache id)) (< cost (array-ref cache id)))
                        (begin
                          (array-set! cache id cost)
                          (priority-queue-insert queue i))
                        queue))))
                (priority-queue-rest queue) (next array cost open goal)))))))))

(define (open lst)
  (let loop ((lst lst))
    (bind (coord _ _ _ use%) (car lst)
      (if (= use% 0)
        coord
        (loop (cdr lst))))))

(define (solve/1 input)
  (count
    (lambda (i)
      (bind (a _ used _ _ b _ _ avail _) (join i)
        (and (not (equal? a b)) (> avail used 0))))
    (power input 2)))

(define (solve/2 input)
  (let* ((dimensions (map add1 (apply map max (map car input)))) (array (make-array dimensions #t)))
    (for-each
      (lambda (i)
        (bind (coord size _ _ _) i
          (when (> size 500)
            (array-set! array coord #f))))
      input)
    (path array (open input) (list (- (car dimensions) 1) 0) (list 0 0))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 985)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 179))))
