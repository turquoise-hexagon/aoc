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

(define (valid-coords? content coords)
  (every
    (lambda (coord)
      (and (not (hash-table-exists? content coord)) (<= 0 (second coord) (- WIDTH 1))))
    coords))

(define (move-rock content coords move)
  (let ((acc (map
               (lambda (coord)
                 (map + coord move))
               coords)))
    (if (valid-coords? content acc)
      acc
      coords)))

(define (identifier content current-height moves-index rocks-index)
  (list moves-index rocks-index
    (filter
      (lambda (coord)
        (hash-table-exists? content (map + coord (list current-height 0))))
      (product
        (range 0 (- SECTION 1))
        (range 0 (- WIDTH   1))))))

(define (solve moves rocks iterations)
  (let ((cache (make-hash-table)) (content (make-hash-table)))

    ;; add a bottom to the tower
    (for-each
      (lambda (i)
        (hash-table-set! content (list 0 i) #t))
      (range 0 (- WIDTH 1)))

    (let loop ((i 0) (current-height 0) (uncycled-height 0) (moves-index 0) (rocks-index 0))
      (if (= i iterations)
        (abs (+ current-height uncycled-height))

        ;; get a fresh rock
        (let*
          ((rock (list-ref rocks rocks-index))
           (height (rock-height rock))
           (coords (rock-coords rock))
           (coords (move-rock content coords (list (- current-height 3 height) OFFSET))))

          (let subloop ((coords coords) (moves-index moves-index))
            ;; move the rock
            (let*
              ((move (list-ref moves moves-index))
               (push (move-rock content coords move))
               (grav (move-rock content push '(1 0)))
               (moves-index (modulo (+ moves-index 1) (length moves))))
              (if (equal? push grav)

                ;; rock settles
                (begin
                  (for-each
                    (lambda (coord)
                      (hash-table-set! content coord #t))
                    push)
                  (let* ((current-height (min current-height (apply min (map first push)))) (id (identifier content current-height moves-index rocks-index)))

                    ;; jump ahread
                    (if (hash-table-exists? cache id)
                      (apply
                        (lambda (old-i old-height)
                          (let* ((diff-i (- i old-i)) (diff-height (- current-height old-height)) (mul (quotient (- iterations i) diff-i)))
                            (loop (+ i (* mul diff-i) 1) current-height (+ uncycled-height (* mul diff-height)) moves-index
                              (modulo (+ rocks-index 1) (length rocks)))))
                        (hash-table-ref cache id))

                      (begin
                        (hash-table-set! cache id (list i current-height))
                        (loop (+ i 1) current-height uncycled-height moves-index
                          (modulo (+ rocks-index 1) (length rocks)))))))
                (subloop grav moves-index)))))))))

(let ((moves (import-input)) (rocks (map parse-rock ROCKS)))
  (print (solve moves rocks 2022))
  (print (solve moves rocks #e1e12)))
