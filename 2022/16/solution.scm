(import
  (chicken io)
  (chicken irregex)
  (euler)
  (srfi 1)
  (srfi 69))

(define regex "^Valve ([A-z]+) has flow rate=([0-9]+); tunnels? leads? to valves? ([A-z, ]+)$")

(define (parse-scan str)
  (apply
    (lambda (name rate tunnels)
      `(,name ,(string->number rate) ,(irregex-split ", " tunnels)))
    (let ((matches (irregex-match regex str)))
      (map
        (lambda (_)
          (irregex-match-substring matches _))
        (iota (irregex-match-num-submatches matches) 1)))))

(define (process-scans! rates dists lst)
  (for-each
    (lambda (_)
      (apply
        (lambda (name rate tunnels)
          (unless (= rate 0)
            (hash-table-set! rates name rate))
          (for-each
            (lambda (_)
              (hash-table-set! dists `(,_ ,name) 1))
            tunnels))
        _))
    lst))

(define (_floyd-warshall! dists a b c)
  (hash-table-set! dists `(,b ,c)
    (min (hash-table-ref/default dists `(,b ,c) #e1e6)
      (+ (hash-table-ref/default dists `(,b ,a) #e1e6)
         (hash-table-ref/default dists `(,a ,c) #e1e6)))))

(define (floyd-warshall! dists lst)
  (for-each
    (lambda (_)
      (apply _floyd-warshall! dists _))
    (product lst lst lst)))

(define (import-input)
  (let ((input (map parse-scan (read-lines)))
        (rates (make-hash-table))
        (dists (make-hash-table)))
    (process-scans! rates dists input) (floyd-warshall! dists (map first input))
    (list rates dists)))

(define (id current time unopened elephant)
  ;; make the hash function's job easier
  (string-append current (number->string time) (apply string-append unopened) (if elephant "1" "0")))

(define cache
  (make-hash-table))

(define (solve start time elephant rates dists)
  (let loop ((current start) (time time) (unopened (hash-table-keys rates)) (elephant elephant))
    (let ((id (id current time unopened elephant)))
      (if (hash-table-exists? cache id)
        (hash-table-ref cache id)
        (let subloop ((lst unopened) (acc 0))
          (if (null? lst)
            (let ((acc (if elephant (max acc (loop "AA" 26 unopened #f)) acc)))
              (hash-table-set! cache id acc)
              acc)
            (let* ((i (car lst)) (dist (hash-table-ref dists `(,current ,i))))
              (subloop (cdr lst)
                (if (< dist time)
                  (let ((time (- time dist 1)))
                    (max acc (+ (* (hash-table-ref rates i) time) (loop i time (delete i unopened) elephant))))
                  acc)))))))))

(let ((input (import-input)))
  (print (apply solve "AA" 30 #f input))
  (print (apply solve "AA" 26 #t input)))
