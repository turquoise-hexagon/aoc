(import
  (chicken io)
  (chicken irregex)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (parse-scanner lst)
  (receive (_ . points) (apply values lst)
    (map (cut map string->number <>)
      (map (cut string-split <> ",") points))))

(define (import-input)
  (map parse-scanner
    (map (cut string-split <> "\n")
      (irregex-split "\n{2}" (read-string #f)))))

(define (rotations point)
  (let ((lst (flatten point (map - point))))
    ;; horrible but reliable
    (receive (x y z a b c) (apply values lst)
      `((,x ,y ,z)
        (,x ,z ,b)
        (,x ,b ,c)
        (,x ,c ,y)
        (,y ,x ,c)
        (,y ,z ,x)
        (,y ,a ,z)
        (,y ,c ,a)
        (,z ,x ,y)
        (,z ,y ,a)
        (,z ,a ,b)
        (,z ,b ,x)
        (,a ,y ,c)
        (,a ,z ,y)
        (,a ,b ,z)
        (,a ,c ,b)
        (,b ,x ,z)
        (,b ,z ,a)
        (,b ,a ,c)
        (,b ,c ,x)
        (,c ,x ,b)
        (,c ,y ,x)
        (,c ,a ,y)
        (,c ,b ,a)))))

(define (translation a b)
  (map - a b))

(define (translate translation points)
  (map (cut map + translation <>) points))

(define (counts lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (item)
        (hash-table-set! acc item
          (+ 1 (hash-table-ref/default acc item 0))))
      lst)
    acc))

(define (match? a b n)
  (call/cc
    (lambda (_)
      (for-each
        (lambda (rotation)
          (hash-table-for-each
            (counts (join (map
                            (lambda (point)
                              (map (cut translation point <>) rotation))
                            a)))
            (lambda (translation count)
              (when (>= count n)
                (_ (list translation (translate translation rotation)))))))
        (apply zip (map rotations b)))
      (_ #f))))

(define (connect/h center scanners translations n)
  (let ((acc (alist->hash-table (map list center))))
    (let loop ((lst scanners) (scanners scanners) (translations translations))
      (if (null? lst)
        (values (hash-table-keys acc) scanners translations)
        (receive (current . lst) (apply values lst)
          (let ((result (match? center current n)))
            (if result
              (receive (translation translated) (apply values result)
                ;; add to the center, remove matched scanner, add translation
                (for-each (cut hash-table-set! acc <> '()) translated)
                (loop lst (delete current scanners) (cons translation translations)))
              (loop lst scanners translations))))))))

(define (connect scanners n)
  (receive (center . scanners) (apply values scanners)
    (let loop ((center center) (scanners scanners) (translations '()))
      (if (null? scanners)
        (values center translations)
        (receive (center scanners translations) (connect/h center scanners translations n)
          (loop center scanners translations))))))

(define (manhattan a b)
  (apply + (map abs (map - a b))))

(let ((input (import-input)))
  (receive (center translations) (connect input 12)
    (print (length center))
    (print (apply max
             (map (cut apply manhattan <>)
               (combinations translations 2))))))
