(import
  (chicken io)
  (chicken string)
  (srfi 1)
  (srfi 69))

(define result
  (alist->hash-table
    '(("children"    . 3)
      ("cats"        . 7)
      ("samoyeds"    . 2)
      ("pomeranians" . 3)
      ("akitas"      . 0)
      ("vizslas"     . 0)
      ("goldfish"    . 5)
      ("trees"       . 3)
      ("cars"        . 2)
      ("perfumes"    . 1))))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (apply
          (lambda (_ id . lst)
            (hash-table-set! acc (string->number id)
              (let ((data (make-hash-table)))
                (for-each
                  (lambda (i)
                    (apply
                      (lambda (id val)
                        (hash-table-set! data id (string->number val)))
                      i))
                  (chop lst 2))
                data)))
          (string-split i " :,")))
      (read-lines))
    acc))

(define-inline (compare? data i ?)
  (? (hash-table-ref data   i)
     (hash-table-ref result i)))

(define (proc/1 data i)
  (compare? data i =))

(define (proc/2 data i)
  (cond
    ((or (string=? i "cats")
         (string=? i "trees"))
     (compare? data i >))
    ((or (string=? i "pomeranians")
         (string=? i "goldfish"))
     (compare? data i <))
    (else
     (compare? data i =))))

(define (solve input proc)
  (find
    (lambda (i)
      (let ((data (hash-table-ref input i)))
        (every
          (lambda (i)
            (proc data i))
          (hash-table-keys data))))
    (hash-table-keys input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 373)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 260))))
