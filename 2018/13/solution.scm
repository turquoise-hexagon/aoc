(import
  (chicken format)
  (chicken io)
  (chicken sort)
  (euler)
  (srfi 1)
  (srfi 69)
  (only (srfi 133) vector-count))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant directions
  #((-1  0)
    ( 0  1)
    ( 1  0)
    ( 0 -1)))

(define (import-input)
  (list->array (map string->list (read-lines))))

(define (carts array)
  (list->vector
    (filter-map
      (lambda (coord)
        (case (array-ref array coord)
          ((#\^) (list 0 0 coord))
          ((#\>) (list 0 1 coord))
          ((#\v) (list 0 2 coord))
          ((#\<) (list 0 3 coord))
          (else  #f)))
      (array-indexes array))))

(define (compare? a b)
  (cond
    ((not b) #t)
    ((not a) #f)
    (else
     (bind (_ _ a b _ _ c d) (flatten a b)
       (cond
         ((< a c) #t)
         ((> a c) #f)
         (else (< b d)))))))

(define (_move count direction coord)
  (list count direction (map + coord (vector-ref directions direction))))

(define (move array cart)
  (bind (count direction coord) cart
    (case (array-ref array coord)
      ((#\+)
       (_move (+ count 1)
         (modulo
           (+ direction
              (case (modulo count 3)
                ((0) -1)
                ((1)  0)
                ((2)  1)))
           4)
         coord))
      ((#\/) (_move count (modulo (+ direction (if (even? direction) 1 -1)) 4) coord))
      ((#\\) (_move count (modulo (+ direction (if (odd?  direction) 1 -1)) 4) coord))
      (else  (_move count direction coord)))))

(define (collision carts)
  (let ((mem (make-hash-table)))
    (let loop ((i 0))
      (if (= i (vector-length carts))
        #f
        (let ((cart (vector-ref carts i)))
          (if cart
            (bind (_ _ coord) cart
              (if (hash-table-exists? mem coord)
                (list i (hash-table-ref mem coord) coord)
                (begin
                  (hash-table-set! mem coord i)
                  (loop (+ i 1)))))
            (loop (+ i 1))))))))

(define (output acc/1 acc/2)
  (map
    (lambda (i)
      (apply format "~a,~a" (reverse (last i))))
    (list acc/1 (vector-ref acc/2 0))))

(define (solve input)
  (let ((carts (carts input)))
    (let main ((acc '()))
      (sort! carts compare?)
      (if (= (vector-count identity carts) 1)
        (output acc carts)
        (let loop ((i 0) (acc acc))
          (if (= i (vector-length carts))
            (main acc)
            (let ((cart (vector-ref carts i)))
              (if cart
                (begin
                  (vector-set! carts i (move input cart))
                  (let ((collision (collision carts)))
                    (if collision
                      (bind (a b coord) collision
                        (vector-set! carts a #f)
                        (vector-set! carts b #f)
                        (loop (+ i 1) (cons coord acc)))
                      (loop (+ i 1) acc))))
                (loop (+ i 1) acc)))))))))

(let ((parts (solve (import-input))))
  (for-each print parts) (equal? parts '("111,13" "16,73")))
