(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define MAGIC 3000)

;; priority-queue helpers
(define (priority-queue-take comp? queue n)
  (let loop ((i 0) (queue queue) (acc priority-queue-empty))
    (if (priority-queue-empty? queue)
      acc
      (if (= i n)
        acc
        (loop (+ i 1) (priority-queue-rest comp? queue)
          (let ((_ (priority-queue-first queue)))
            (priority-queue-insert comp? _ acc)))))))

(define (priority-queue-fold comp? queue proc acc)
  (let loop ((queue queue) (acc acc))
    (if (priority-queue-empty? queue)
      acc
      (loop (priority-queue-rest comp? queue)
        (proc acc (priority-queue-first queue))))))

(define (parse-blueprint str)
  (apply
    (lambda (name a b c d e f)
      `(((0  0  0 ,a) (0 0 0 1))
        ((0  0  0 ,b) (0 0 1 0))
        ((0  0 ,d ,c) (0 1 0 0))
        ((0 ,f  0 ,e) (1 0 0 0))
        ((0  0  0  0) (0 0 0 0))))
    (filter-map string->number
      (string-split str " :"))))

(define (import-input)
  (map parse-blueprint (read-lines)))

(define (_comp? a b)
  (cond
    ((or (null? a)
         (null? b))
     #t)
    ((< (car a) (car b)) #t)
    ((> (car a) (car b)) #f)
    (else
     (_comp?
       (cdr a)
       (cdr b)))))

(define (comp? a b)
  (_comp?
    (third b)
    (third a)))

(define (search lst n)
  (let ((cache (make-hash-table))) ;; avoid repeated computation
    (priority-queue-first
      (foldl
        (lambda (acc _)
          (priority-queue-take comp? ;; prune the queue
            (priority-queue-fold comp? acc
              (lambda (acc _)
                (apply
                  (lambda (resources robots total)
                    (foldl
                      (lambda (acc _)
                        (apply
                          (lambda (cost addition)
                            (if (every >= resources cost)
                              (let* ((_ (list (map + resources (map - robots cost)) (map + robots addition) (map + robots total))) (id (list->number (flatten _))))
                                (if (hash-table-exists? cache id) ;; cache
                                  acc
                                  (begin
                                    (hash-table-set! cache id #t)
                                    (priority-queue-insert comp? _ acc))))
                              acc))
                          _))
                      acc lst))
                  _))
              priority-queue-empty)
            MAGIC))
        (priority-queue-insert comp? '((0 0 0 0) (0 0 0 1) (0 0 0 0)) priority-queue-empty)
        (iota n)))))

(define (solve/1 input)
  (apply +
    (map
      (lambda (_ i)
        (* i (caar (search _ 24))))
      input (iota (length input) 1))))

(define (solve/2 input)
  (apply *
    (map
      (lambda (_)
        (caar (search _ 32)))
      (take input 3))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
