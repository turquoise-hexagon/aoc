(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant L 0)
(define-constant H 1)

(define (parse-name str)
  (case (string-ref str 0)
    ((#\&) (values (substring str 1) '&))
    ((#\%) (values (substring str 1) '%))
    (else  (values str 'B))))

(define (fix! table)
  (hash-table-for-each table
    (lambda (name data)
      (bind (_ _ destinations) data
        (for-each
          (lambda (destination)
            (when (hash-table-exists? table destination)
              (bind (type state _) (hash-table-ref table destination)
                (case type
                  ((&) (hash-table-set! state name L))))))
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
    acc))

(define (split lst item)
  (bind (_ name _) item
    (let loop ((lst lst) (acc '()))
      (if (null? lst)
        (values (reverse acc) lst)
        (bind (_ match _) (car lst)
          (if (string=? name match)
            (loop (cdr lst) (cons (car lst) acc))
            (values (reverse acc) lst)))))))

(define-inline (iterate value)
  (subloop (cdr todo)
    (foldl
      (lambda (stack destination)
        (cons (list destination name value) stack))
      stack destinations)))

(define (destinations? table name match)
  (bind (_ _ destinations) (hash-table-ref table name)
    (member match destinations)))

(define (analyse table)
  (let ((parent
          (find
            (lambda (name)
              (destinations? table name "rx"))
            (hash-table-keys table))))
    (filter
      (lambda (name)
        (destinations? table name parent))
      (hash-table-keys table))))

(define (solve table iterations analysis)
  (call/cc
    (lambda (return)
      (let
        ((counts (make-hash-table))
         (cycles (make-hash-table)))
        (let main ((i 0))
          (let loop ((stack (list (list "broadcaster" "button" L))))
            (unless (null? stack)
              (let-values (((todo stack) (split stack (car stack))))
                (let subloop ((todo todo) (stack stack))
                  (if (null? todo)
                    (loop stack)
                    (bind (name sender value) (car todo)
                      (when (and (member name analysis) (= value L))
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
                             (iterate (if (every (cut = <> H) (hash-table-values state)) L H)))
                            ((%)
                             (if (= value H)
                               (subloop (cdr todo) stack)
                               (begin
                                 (hash-table-set! table name (list type (not state) destinations))
                                 (iterate (if state L H)))))
                            ((B) (iterate value))))
                        (subloop (cdr todo) stack))))))))
          (main (+ i 1)))))))

(let ((input (import-input)))
  (let ((parts (solve input 1000 (analyse input))))
    (for-each print parts)
    (assert (equal? parts '(812721756 7284368133192)))))
