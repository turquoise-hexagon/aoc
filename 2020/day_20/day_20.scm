(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (chicken string)
        (matchable)
        (srfi 69)
        (srfi 1))

(define sea-monster
  (map string->list (list "                  # "
                          "#    ##    ##    ###"
                          " #  #  #  #  #  #   ")))

(define (chunk->tile chunk)
  (match (string-split chunk "\n")
    ((name . content)
     (list (string->number (car (irregex-extract "[0-9]+" name))) (map string->list content)))))

(define (import-input path)
  (map chunk->tile (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (rotate-tile tile)
  (match tile
    ((name content)
     (list name (fold
                  (lambda (a acc)
                    (cons (map (cut list-ref <> a) content) acc))
                  (list) (iota (length (list-ref content 0))))))))

(define (flip-tile tile)
  (match tile ((name content) (list name (reverse content)))))

(define (generate-tile-permutations a)
  (let* ((b (rotate-tile a))
         (c (rotate-tile b))
         (d (rotate-tile c))
         (e (flip-tile   a))
         (f (rotate-tile e))
         (g (rotate-tile f))
         (h (rotate-tile g)))
    (list a b c d e f g h)))

(define (get-border tile arg)
  (match tile
    ((_ content)
     (case arg
       ((t) (car content))
       ((b) (car (reverse content)))
       ((l) (map (cut list-ref <> 0) content))
       ((r) (map (cut list-ref <> 0) (map reverse content)))))))

(define (match-border border proc permutations)
  (fold
    (lambda (a acc)
      (if (equal? border (proc a)) a acc))
    (list) permutations))

(define (match-tile a b)
  (let ((permutations (generate-tile-permutations b)))
    (call/cc
      (lambda (return)
        (let ((match-tile/h
                (lambda (border proc offsets)
                  (let ((result (match-border border proc permutations)))
                    (unless (null? result)
                      (return (list offsets result)))))))
          (match-tile/h (get-border a 't) (cut get-border <> 'b) '(-1  0))
          (match-tile/h (get-border a 'b) (cut get-border <> 't) '(+1  0))
          (match-tile/h (get-border a 'l) (cut get-border <> 'r) '( 0 -1))
          (match-tile/h (get-border a 'r) (cut get-border <> 'l) '( 0 +1))
          (return (list)))))))

(define (match-tiles input)
  (match input
    ((head . tail)
     (let ((hash (make-hash-table)) (todo (make-hash-table)))
       (for-each
         (cut hash-table-set! todo <> 0)
         (map car tail))
       (hash-table-set! hash head '(0 0))
       (let match-tile/h ()
         (hash-table-for-each hash
           (lambda (hash-key hash-value)
             (match hash-value
               ((x y)
                (for-each
                  (lambda (todo-key)
                    (match (match-tile hash-key (assoc todo-key input))
                      (((a b) tile) (hash-table-delete! todo todo-key) (hash-table-set! hash tile `(,(+ x a) ,(+ y b))))
                      (_ void)))
                  (hash-table-keys todo))))))
         (unless (null? (hash-table-keys todo))
           (match-tile/h)))
       hash))))

(define (count-neighbors item lst)
  (let ((offsets '((-1 0) (+1 0) (0 -1) (0 +1))))
    (count
      (cut member <> lst)
      (map
        (lambda (offset)
          (fold-right
            (lambda (a b acc)
              (cons (+ a b) acc))
            (list) item offset))
        offsets))))

(define (solve/1 matched-tiles)
  (let ((lst (hash-table-values matched-tiles)))
    (print (apply * (fold
                      (lambda (a acc)
                        (match a
                          ((key val)
                           (if (= 2 (count-neighbors val lst))
                               (cons key acc)
                               acc))))
                      (list) (hash-table-map matched-tiles
                               (lambda (key val)
                                 (list (car key) val))))))))

(define (cut-tile-to-size tile)
  (match tile
    ((name content)
     (let ((h (length content)) (w (length (list-ref content 0))))
       (list name (fold-right
                    (lambda (a acc)
                      (cons (take (drop a 1) (- w 2)) acc))
                    (list) (take (drop content 1) (- h 2))))))))

(define (fix-matched-tiles matched-tiles)
  (let* ((lst (hash-table-values matched-tiles))
         (h-min (apply min (map car  lst)))
         (w-min (apply min (map cadr lst))))
    (let ((hash (make-hash-table)))
      (hash-table-for-each matched-tiles
        (lambda (tile coordinates)
          (match coordinates
            ((x y) (hash-table-set! hash (cut-tile-to-size tile) `(,(- x h-min) ,(- y w-min)))))))
      hash)))

(define (allocate-vector matched-tiles)
  (let* ((lst (hash-table-values matched-tiles))
         (h-max (apply max (map car  lst)))
         (w-max (apply max (map cadr lst))))
    (match (car (hash-table-keys matched-tiles))
      ((_ content) (let* ((h (length content)) (w (length (list-ref content 0))) (array (make-vector (* h (+ h-max 1)))))
                     (for-each
                       (cut vector-set! array <> (make-vector (* w (+ w-max 1))))
                       (iota (* h (+ h-max 1))))
                     array)))))

(define (put-image-together matched-tiles)
  (let* ((fixed-tiles (fix-matched-tiles matched-tiles)) (array (allocate-vector fixed-tiles)))
    (hash-table-for-each fixed-tiles
      (lambda (tile coordinates)
        (match-let
          (((x y) coordinates)
           ((_ content) tile))
          (let ((h (length content)) (w (length (car content))))
            (for-each
              (lambda (i)
                (for-each
                  (lambda (j)
                    (vector-set! (vector-ref array (+ (* x h) i)) (+ (* y w) j) (list-ref (list-ref content i) j)))
                  (iota w)))
              (iota h))))))
    (list 0 (map vector->list (vector->list array)))))

(define (check-for-sea-monster content i j)
  (let ((h (length sea-monster)) (w (length (list-ref sea-monster 0))))
    (fold
      (lambda (a acc)
        (and acc (fold
                   (lambda (b acc)
                     (if (and (char=? #\# (list-ref (list-ref sea-monster a) b))
                              (char=? #\. (list-ref (list-ref content (+ i a)) (+ j b))))
                         #f
                         acc))
                   #t (iota w))))
      #t (iota h))))

(define (count-sea-monsters tile)
  (match tile
    ((_ content)
     (let ((h (length content)) (w (length (list-ref content 0))))
       (fold
         (lambda (a acc)
           (+ acc (fold
                    (lambda (b acc)
                      (if (check-for-sea-monster content a b)
                          (+ acc 1)
                          acc))
                    0 (iota (- w (length (list-ref sea-monster 0)))))))
         0 (iota (- h (length sea-monster))))))))

(define (water-roughness tile)
  (define (water-roughness/h lst)
    (fold
      (lambda (a acc)
        (+ acc (count (cut char=? #\# <>) a)))
      0 lst))
  (match tile
    ((_ content) (- (water-roughness/h content) (* (count-sea-monsters tile) (water-roughness/h sea-monster))))))

(define (solve/2 image)
  (print (apply min (map water-roughness (generate-tile-permutations image)))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (let ((matched-tiles (match-tiles input)))
      (solve/1 matched-tiles)
      (let ((image (put-image-together matched-tiles)))
        (solve/2 image)))))
