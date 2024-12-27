(import
  (chicken io)
  (chicken sort)
  (chicken string)
  (euler-syntax)
  (srfi 1)
  (srfi 69))

(define (connect! table a b)
  (hash-table-update!/default table a
    (lambda (tmp)
      (hash-table-set! tmp b #t)
      tmp)
    (make-hash-table)))

(define (import-input)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (i)
        (bind (a b) (string-split i "-")
          (connect! acc a b)
          (connect! acc b a)))
      (read-lines))
    acc))

(define (group? table node group)
  (every
    (lambda (i)
      (hash-table-exists? (hash-table-ref table node) i))
    group))

(define (solve/1 table)
  (let ((nodes (hash-table-keys table)))
    (let loop ((cnt 1) (acc (map list (filter (lambda (i) (substring=? i "t")) nodes))))
      (if (= cnt 3)
        (length (delete-duplicates (map (lambda (i) (sort i string<?)) acc)))
        (loop (+ cnt 1) (append-map (lambda (group) (map (lambda (node) (cons node group)) (filter (lambda (node) (group? table node group)) nodes))) acc))))))

(define (solve/2 table)
  (string-intersperse
    (sort
      (let loop ((cnt 0) (group '()) (nodes (hash-table-keys table)))
        (if (null? nodes)
          group
          (let ((head (car nodes))
                (tail (cdr nodes)))
            (if (group? table head group)
              (let ((a (loop (+ cnt 1) (cons head group) tail)) (b (loop cnt group tail)))
                (if (> (length a)
                       (length b))
                  a
                  b))
              (loop cnt group tail)))))
      string<?)
    ","))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 1302)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (string=? part/2 "cb,df,fo,ho,kk,nw,ox,pq,rt,sf,tq,wi,xz"))))
