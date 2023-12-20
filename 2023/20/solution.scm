(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (parse-name str)
  (case (string-ref str 0)
    ((#\&) (values (substring str 1) '&))
    ((#\%) (values (substring str 1) '%))
    (else  (values str 'B))))

(define (analyse table)
  (let ((parent
          (find
            (lambda (name)
              (bind (_ _ match) (hash-table-ref table name) (member "rx" match)))
            (hash-table-keys table))))
    (filter
      (lambda (name)
        (bind (_ _ match) (hash-table-ref table name) (member parent match)))
      (hash-table-keys table))))

(define (fix! table)
  (hash-table-for-each table
    (lambda (name data)
      (bind (_ _ destinations) data
        (for-each
          (lambda (destination)
            (when (hash-table-exists? table destination)
              (bind (type state _) (hash-table-ref table destination)
                (case type
                  ((&) (hash-table-set! state name 0))))))
          destinations)))))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (name . destinations) (string-split i " ->,")
          (let-values (((name type) (parse-name name)))
            (hash-table-set! acc name
              (case type
                ((&) (list type (make-hash-table) destinations))
                ((%) (list type #f destinations))
                ((B) (list type #f destinations)))))))
      (read-lines))
    (fix! acc)
    (values acc (analyse acc))))

(define (compare? a b)
  (< (car a)
     (car b)))

(define-inline (iterate value)
  (foldl
    (lambda (queue destination)
      (priority-queue-insert queue (list (+ priority 1) destination name value)))
    (priority-queue-rest queue) destinations))

(define (solve table iterations analysis)
  (call/cc
    (lambda (return)
      (let
        ((counts (make-hash-table))
         (cycles (make-hash-table)))
        (do ((i 1 (+ i 1))) (#f)
          (do ((queue (list->priority-queue (list (list 0 "broadcaster" "button" 0)) compare?)
                 (bind (priority name sender value) (priority-queue-first queue)
                   (when (and (member name analysis) (= value 0))
                     (hash-table-set! cycles name i)
                     (when (every (cut hash-table-exists? cycles <>) analysis)
                       (return
                         (list
                           (apply * (hash-table-values counts))
                           (apply * (hash-table-values cycles))))))
                   (unless (> i iterations)
                     (hash-table-update!/default counts value add1 0))
                   (if (hash-table-exists? table name)
                     (bind (type state destinations) (hash-table-ref table name)
                       (case type
                         ((&)
                          (hash-table-set! state sender value)
                          (iterate (if (every (cut = <> 1) (hash-table-values state)) 0 1)))
                         ((%)
                          (if (= value 1)
                            (priority-queue-rest queue)
                            (begin
                              (hash-table-set! table name (list type (not state) destinations))
                              (iterate (if state 0 1)))))
                         ((B) (iterate value))))
                     (priority-queue-rest queue)))))
            ((priority-queue-empty? queue))))))))

(let-values (((input analysis) (import-input)))
  (let ((parts (solve input 1000 analysis)))
    (for-each print parts)
    (assert (equal? parts '(812721756 233338595643977)))))
