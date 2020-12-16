(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (matchable)
        (srfi 1)
        (srfi 69))

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
                (fold
                  (lambda (a acc)
                    (match a
                      (($ field _ items)
                       (append items acc))))
                  (list) fields)))))

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
     (print (apply + (map (cut apply + <>) (filter list? (map (cut is-valid-ticket? <> fields) others))))))))

(define (list-valid-tickets fields yours others)
  (fold
    (lambda (a acc)
      (if (boolean? (is-valid-ticket? a fields))
          (cons a acc)
          acc))
    (list) (cons (car yours) others)))

(define (list-possible-matches column fields)
  (let list-possible-matches/h ((fields fields) (acc (list)))
    (if (null? fields)
        acc
        (list-possible-matches/h (cdr fields) (let ((field (car fields)))
                                                (if (boolean? (is-valid-ticket? column (list field)))
                                                    (cons field acc)
                                                    acc))))))

(define (list-matches fields valid-tickets)
  (let ((matches (make-hash-table))
        (unmatched-fields  (make-hash-table))
        (unmatched-columns (make-hash-table)))
    (for-each (lambda (field)  (hash-table-set! unmatched-fields  field  0)) fields)
    (for-each (lambda (column) (hash-table-set! unmatched-columns column 0)) (iota (length (car valid-tickets))))
    (let list-matches/h ()
      (if (null? (hash-table-keys unmatched-columns))
          matches
          (begin
            (for-each
              (lambda (column)
                (match (list-possible-matches (map (cut list-ref <> column) valid-tickets) (hash-table-keys unmatched-fields))
                  ((field)
                   (hash-table-delete! unmatched-fields  field)
                   (hash-table-delete! unmatched-columns column)
                   (hash-table-set! matches field column))
                  (_ void)))
            (hash-table-keys unmatched-columns))
            (list-matches/h))))))

(define (solve/2 input)
  (match input
    ((fields yours others)
     (let ((matches (list-matches fields (list-valid-tickets fields yours others))))
       (print (apply * (map (cut list-ref (car yours) <>)
                            (fold
                              (lambda (a acc)
                                (if (irregex-match? "departure.*" (field-name a))
                                    (cons (hash-table-ref matches a) acc)
                                    acc))
                              (list) (hash-table-keys matches)))))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
