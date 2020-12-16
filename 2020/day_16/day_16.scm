(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (matchable)
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
                (apply append (map field-items fields))))))

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
     (print (apply + (is-valid-ticket? (apply append others) fields))))))

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
  (let list-matches/h ((fields fields) (columns (iota (length (car valid-tickets)))) (acc (list)))
    (call/cc
      (lambda (return)
        (for-each
          (lambda (column)
            (match (list-possible-matches (map (cut list-ref <> column) valid-tickets) fields)
              ((field)
               (return (list-matches/h (delete field fields) (delete column columns) (cons (list field column) acc))))
              (_ void)))
          columns)
        (return acc)))))

(define (solve/2 input)
  (match input
    ((fields yours others)
     (let ((matches (list-matches fields (list-valid-tickets fields yours others))))
       (print (apply * (map (cut list-ref (car yours) <>)
                            (fold
                              (lambda (a acc)
                                (match a
                                  ((field position)
                                   (if (irregex-match? "departure.*" (field-name field))
                                       (cons position acc)
                                       acc))))
                              (list) matches))))))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
