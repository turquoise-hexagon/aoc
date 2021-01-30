(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (matchable)
        (srfi 69)
        (srfi 1))

(define-record field name items)

(define (chunk->fields chunk)
  (map
    (lambda (field-chunk)
      (match (irregex-split ": " field-chunk)
        ((name unparsed-items)
         (make-field name
                     (map
                       (lambda (unparsed-item)
                         (map string->number (irregex-split "-" unparsed-item)))
                       (irregex-split " or " unparsed-items))))))
    (irregex-split "\n" chunk)))

(define (chunk->tickets chunk)
  (map
    (lambda (unparsed-ticket)
      (map string->number (irregex-split "," unparsed-ticket)))
    (cdr (irregex-split "\n" chunk))))

(define (import-input path)
  (match (irregex-split "\n\n" (read-string #f (open-input-file path)))
    ((fields-chunk yours-chunk others-chunk)
     (list (chunk->fields fields-chunk)
           (chunk->tickets yours-chunk)
           (chunk->tickets others-chunk)))))

(define (is-valid-value? value fields)
  (not (null? (filter
                (lambda (constraint)
                  (match constraint
                    ((lower higher)
                     (<= lower value higher))))
                (concatenate (map field-items fields))))))

(define (is-valid-ticket? ticket fields)
  (let ((result (fold
                  (lambda (a acc)
                    (if (is-valid-value? a fields)
                        acc
                        (cons a acc)))
                  (list) ticket)))
    (if (null? result)
        #t
        result)))

(define (solve/1 input)
  (match input
    ((fields yours others)
     (print (apply + (is-valid-ticket? (concatenate others) fields))))))

(define (list-valid-tickets fields yours others)
  (fold
    (lambda (a acc)
      (if (boolean? (is-valid-ticket? a fields))
          (cons a acc)
          acc))
    (list) (cons (car yours) others)))

(define (list-possible-matches fields valid-tickets)
  (let ((hash (make-hash-table)))
    (for-each
      (lambda (field)
        (for-each
          (lambda (index)
            (when (boolean? (is-valid-ticket? (map (cut list-ref <> index) valid-tickets) (list field)))
              (hash-table-set! hash field (cons index (hash-table-ref/default hash field (list))))))
          (iota (length (car valid-tickets)))))
      fields)
    hash))

(define (list-matches fields valid-tickets)
  (let ((hash (list-possible-matches fields valid-tickets)))
    (let list-matches/h ()
      (let* ((vals (hash-table-values hash)) (uniques (map car (filter (lambda (val) (= 1 (length val))) vals))))
        (for-each
          (lambda (unique)
            (hash-table-for-each hash
              (lambda (key val)
                (unless (= 1 (length val))
                  (hash-table-set! hash key (delete unique val))))))
          uniques)
        (unless (= (length uniques) (length vals))
          (list-matches/h))))
    hash))

(define (solve/2 input)
  (match input
    ((fields yours others)
     (print (apply * (map (cut list-ref (car yours) <>)
                          (hash-table-fold (list-matches fields (list-valid-tickets fields yours others))
                            (lambda (key value acc)
                              (if (irregex-match? "departure.*" (field-name key))
                                  (cons (car value) acc)
                                  acc))
                            (list))))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
