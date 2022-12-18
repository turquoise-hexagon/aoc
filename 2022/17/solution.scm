(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define-record rock
  height
  coords)

(define WIDTH 7)
(define OFFSET 2)
(define SECTION 30)

(define ROCKS 
  '(("####")

    (".#."
     "###"
     ".#.")

    ("..#"
     "..#"
     "###")

    ("#"
     "#"
     "#"
     "#")

    ("##"
     "##")))

(define (import-input)
  (map
    (lambda (char)
      (case char
        ((#\>) '(0  1))
        ((#\<) '(0 -1))))
    (string->list (read-line))))

(define (parse-rock lst)
  (make-rock (length lst)
    (let ((array (list->array (map string->list lst))))
      (filter
        (lambda (coord)
          (char=? (array-ref array coord) #\#))
        (array-indexes array)))))

(define (valid-coords? mem coords)
  (every
    (lambda (coord)
      (and (not (hash-table-exists? mem coord)) (<= 0 (second coord) (- WIDTH 1))))
    coords))

(define (move-rock mem coords move)
  (let ((acc (map
               (lambda (coord)
                 (map + coord move))
               coords)))
    (if (valid-coords? mem acc)
      acc
      coords)))

(define (identifier mem h moves-i rocks-i)
  (list moves-i rocks-i
    (filter
      (lambda (coord)
        (hash-table-exists? mem (map + coord (list h 0))))
      (product
        (range 0 (- SECTION 1))
        (range 0 (- WIDTH   1))))))

(define (solve moves rocks n)
  (let ((cache (make-hash-table)) (mem (make-hash-table)))

    ;; add a bottom to the tower
    (for-each
      (lambda (i)
        (hash-table-set! mem (list 0 i) #t))
      (range 0 (- WIDTH 1)))

    (let loop ((i 0) (h 0) (added-h 0) (moves-i 0) (rocks-i 0))
      (if (= i n)
        (abs (+ h added-h))

        ;; get a fresh rock
        (let*
          ((rock (list-ref rocks rocks-i))
           (height (rock-height rock))
           (coords (rock-coords rock))
           (coords (move-rock mem coords (list (- h 3 height) OFFSET))))

          (let subloop ((coords coords) (moves-i moves-i))
            ;; move the rock
            (let*
              ((move (list-ref moves moves-i))
               (push (move-rock mem coords move))
               (grav (move-rock mem push '(1 0)))
               (moves-i (modulo (+ moves-i 1) (length moves))))
              (if (equal? push grav)

                ;; settle the rock
                (begin
                  (for-each
                    (lambda (coord)
                      (hash-table-set! mem coord #t))
                    push)
                  (let* ((h (apply min h (map first push))) (id (identifier mem h moves-i rocks-i)))

                    (if (hash-table-exists? cache id)
                      ;; jump ahead
                      (apply
                        (lambda (old-i old-h)
                          (let*
                            ((diff-i (- i old-i))
                             (diff-h (- h old-h))
                             (mul (quotient (- n i) diff-i)))
                            (loop (+ i (* mul diff-i) 1) h (+ added-h (* mul diff-h)) moves-i (modulo (+ rocks-i 1) (length rocks)))))
                        (hash-table-ref cache id))

                      (begin
                        (hash-table-set! cache id (list i h))
                        (loop (+ i 1) h added-h moves-i (modulo (+ rocks-i 1) (length rocks)))))))
                (subloop grav moves-i)))))))))

(let ((moves (import-input)) (rocks (map parse-rock ROCKS)))
  (print (solve moves rocks 2022))
  (print (solve moves rocks #e1e12)))
