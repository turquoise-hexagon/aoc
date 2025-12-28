(import
  (chicken io)
  (chicken string)
  (euler-syntax)
  (euler)
  (srfi 1))

(define (parse-regions regions)
  (map
    (lambda (i)
      (map string->number (string-split i "x: ")))
    regions))

(define (shape-rotate shape)
  (apply map list (reverse shape)))

(define (shape-flip shape)
  (map reverse shape))

(define (shape-permutations shape)
  (let* ((a shape)
         (b (shape-rotate a))
         (c (shape-rotate b))
         (d (shape-rotate c))
         (e (shape-flip   d))
         (f (shape-rotate e))
         (g (shape-rotate f))
         (h (shape-rotate g)))
    (list a b c d e f g h)))

(define (parse-shapes shapes)
  (map
    (lambda (i)
      (bind (_ . lst) (reverse i)
        (map list->array (delete-duplicates (shape-permutations (map string->list lst)) equal?))))
    (reverse shapes)))

(define (import-input)
  (bind (regions . shapes)
    (foldl
      (lambda (acc i)
        (if (string=? i "") (cons '() acc)
          (cons (cons i (car acc)) (cdr acc))))
      '(()) (read-lines))
    (values
      (parse-regions regions)
      (parse-shapes shapes))))

(define (easy-check?/1 w h todos shapes)
  (> (apply +
       (map
         (lambda (shapes todo)
           (let ((shape (car shapes)))
             (* (count
                  (lambda (i)
                    (char=? (array-ref shape i) #\#))
                  (array-indexes shape))
                todo)))
         shapes todos))
     (* w h)))

(define (easy-check?/2 w h todos shapes)
  (<= (apply +
         (map
           (lambda (shapes todo)
             (* (apply * (array-dimensions (car shapes))) todo))
           shapes todos))
      (* w h)))

(define (valid-coords region shape)
  (let ((temp (array-indexes shape)))
    (filter
      (lambda (a)
        (every
          (lambda (b)
            (if (char=? (array-ref shape b) #\#)
              (let ((c (map + a b)))
                (and (array-exists? region c) (char=? (array-ref region c) #\.)))
              #t))
          temp))
      (array-indexes region))))

(define (region-set! region shape coord)
  (for-each
    (lambda (i)
      (when (char=? (array-ref shape i) #\#)
        (array-set! region (map + i coord) #\#)))
    (array-indexes shape)))

(define (region-unset! region shape coord)
  (for-each
    (lambda (i)
      (when (char=? (array-ref shape i) #\#)
        (array-set! region (map + i coord) #\.)))
    (array-indexes shape)))

(define (valid? region shapes)
  (bind (w h . todos) region
    (cond
      ((easy-check?/1 w h todos shapes) #f)
      ((easy-check?/2 w h todos shapes) #t)
      (else
       (let ((region (make-array (list h w) #\.)))
         (let loop ((todos todos) (shapes shapes))
           (if (null? todos)
             #t
             (let ((head (car todos))
                   (tail (cdr todos)))
               (if (= head 0)
                 (loop (cdr todos) (cdr shapes))
                 (let ((next (cons (- head 1) tail)))
                   (any
                     (lambda (shape)
                       (any
                         (lambda (coord)
                           (region-set! region shape coord)
                           (if (loop next shapes) #t
                             (begin
                               (region-unset! region shape coord)
                               #f)))
                         (valid-coords region shape)))
                     (car shapes))))))))))))

(define (solve regions shapes)
  (count
    (lambda (region)
      (valid? region shapes))
    regions))

(let-values (((regions shapes) (import-input)))
  (let ((part (solve regions shapes)))
    (print part) (assert (= part 512))))
