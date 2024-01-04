(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define (import-input)
  (map
    (lambda (i)
      (map
        (lambda (i)
          (irregex-split " " i))
        (irregex-extract "([a-z]+ (generator|microchip))" (irregex-replace/all "-compatible" i ""))))
    (read-lines)))

(define (compare? a b)
  (< (car a)
     (car b)))

(define (id elevator content)
  (string-append
    (number->string elevator)
    (string-intersperse
      (map
        (lambda (subcontent)
          (string-intersperse (sort (map second subcontent) string<?) ","))
        content))))

(define (valid? subcontent)
  (let ((acc (make-hash-table #:initial '())))
    (for-each
      (lambda (item)
        (bind (element type) item
          (hash-table-update! acc element (cut cons type <>))))
      subcontent)
    (or (every (cut member   "generator"  <>) (hash-table-values acc))
        (every (cut equal? '("microchip") <>) (hash-table-values acc)))))

(define (next cost elevator content)
  (join
    (map
      (lambda (selection)
        (filter-map
          (lambda (offset)
            (let ((next-elevator (+ elevator offset)))
              (if (< -1 next-elevator (length content))
                (let
                  ((subcontent+selection
                     (append selection (list-ref content next-elevator)))
                   (subcontent-selection
                     (foldl
                       (lambda (subcontent-selection i)
                         (delete-first subcontent-selection i equal?))
                       (list-ref content elevator) selection)))
                  (if (and (valid? subcontent+selection)
                           (valid? subcontent-selection))
                    (let*
                      ((content (delete-at content      elevator))
                       (content (insert-at content      elevator subcontent-selection))
                       (content (delete-at content next-elevator))
                       (content (insert-at content next-elevator subcontent+selection)))
                      (list (+ cost 1) next-elevator content))
                    #f))
                #f)))
          '(-1 +1)))
      (filter
        (lambda (selection)
          (<= 1 (length selection) 2))
        (powerset (list-ref content elevator))))))

(define (edit input)
  (cons
    (append (car input)
      '(("elerium"   "generator")
        ("elerium"   "microchip")
        ("dilithium" "generator")
        ("dilithium" "microchip")))
    (cdr input)))

(define (solve input)
  (let ((cache (make-hash-table)))
    (let loop ((queue (priority-queue-insert (priority-queue compare?) (list 0 0 input))))
      (if (priority-queue-empty? queue)
        cache
        (bind (cost elevator content) (priority-queue-first queue)
          (if (every null? (butlast content))
            cost
            (loop
              (foldl
                (lambda (queue next)
                  (bind (cost elevator content) next
                    (let ((id (id elevator content)))
                      (if (or (not (hash-table-exists? cache id)) (< cost (hash-table-ref cache id)))
                        (begin
                          (hash-table-set! cache id cost)
                          (priority-queue-insert queue next))
                        queue))))
                (priority-queue-rest queue) (next cost elevator content)))))))))

(let ((input (import-input)))
  (let ((part/1 (solve input)))
    (print part/1) (assert (= part/1 33)))
  (let ((part/2 (solve (edit input))))
    (print part/2) (assert (= part/2 57))))
