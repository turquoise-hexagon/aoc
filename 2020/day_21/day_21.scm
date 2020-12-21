(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (chicken sort)
        (matchable)
        (srfi 128)
        (srfi 113)
        (srfi 69)
        (srfi 1))

(define (import-input path)
  (map
    (lambda (str)
      (match (irregex-split " \\(contains " str)
        ((a b) (list (string-split a " ") (string-split b " ,)")))))
    (read-lines (open-input-file path))))

(define (allergen-ingredients input)
  (let ((comparator (make-default-comparator)))
    (define (allergen-ingredients/h allergen)
      (set->list (apply set-intersection
                        (fold
                          (lambda (lst acc)
                            (match lst
                              ((a b)
                               (if (member allergen b)
                                   (cons (list->set comparator a) acc)
                                   acc))))
                          (list) input))))
    (let ((hash (make-hash-table)))
      (for-each
        (lambda (allergen)
          (hash-table-set! hash allergen (allergen-ingredients/h allergen)))
        (apply append (map cadr input)))
      hash)))

(define (identify-allergens hash)
  (let identify-allergens/h ()
    (let* ((vals (hash-table-values hash)) (uniques (map car (filter (lambda (val) (= 1 (length val))) vals))))
      (for-each
        (lambda (unique)
          (hash-table-for-each hash
            (lambda (key val)
              (unless (= 1 (length val))
                (hash-table-set! hash key (delete unique val))))))
        uniques)
      (unless (= (length vals) (length uniques))
        (identify-allergens/h)))))

(define (solve/1 input hash)
  (let ((allergens (map car (hash-table-values hash))))
    (print (length (filter
                     (lambda (ingredient)
                       (not (member ingredient allergens)))
                     (apply append (map car input)))))))

(define (solve/2 hash)
  (let ((sorted (map cadr (sort (hash-table-map hash
                                  (lambda (key val)
                                    (list key (car val))))
                                (lambda (a b)
                                  (string<? (car a) (car b)))))))
    (print (string-intersperse sorted ","))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (let ((hash (allergen-ingredients input)))
      (identify-allergens hash)
      (solve/1 input hash)
      (solve/2 hash))))
