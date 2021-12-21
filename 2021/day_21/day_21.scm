(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1)
  (srfi 69))

(define (parse-player str)
  (string->number (last (string-split str " "))))

(define (import-input)
  (values (parse-player (read-line))
          (parse-player (read-line))))

;; part 1 rolls
(define DIE-STATE 0)

(define (die)
  (define (roll)
    (set! DIE-STATE (modulo (+ DIE-STATE 1) 100))
    DIE-STATE)
  (+ (roll) (roll) (roll)))

;; part 2 rolls
(define (counts lst)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (item)
        (hash-table-set! acc item
          (+ (hash-table-ref/default acc item 0) 1)))
      lst)
    (hash-table-fold acc
      (lambda (item count acc)
        (cons (list item count) acc))
      '())))

(define dirac
  (counts (map (cut apply + <>) (product '(1 2 3) '(1 2 3) '(1 2 3)))))

(define (game/1 a b)
  (define (loop a b score/a score/b count)
    (if (or (>= score/a 1000)
            (>= score/b 1000))
      (* (min score/a score/b) count)
      (let ((a (+ (modulo (+ a -1 (die)) 10) 1)))
        (loop b a score/b (+ score/a a) (+ count 3)))))
  (loop a b 0 0 0))

(define (game/2 a b)
  (receive (rolls counts) (unzip2 dirac)
    (let ((acc (make-hash-table)))
      (define (loop a b score/a score/b turn)
        (let ((key (list a b score/a score/b turn)))
          (cond
            ((hash-table-exists? acc key)
             (hash-table-ref     acc key))
            ((>= score/a 21) '(1 0))
            ((>= score/b 21) '(0 1))
            (else
              (let ((result
                      (fold
                        (lambda (roll count acc)
                          (map + acc
                            (map * `(,count ,count)
                              (if (= turn 0)
                                (let ((a (+ (modulo (+ a -1 roll) 10) 1))) (loop a b (+ score/a a) score/b 1))
                                (let ((b (+ (modulo (+ b -1 roll) 10) 1))) (loop a b score/a (+ score/b b) 0))))))
                        '(0 0) rolls counts)))
                (hash-table-set! acc key result)
                result)))))
      (apply max (loop a b 0 0 0)))))

(receive (a b) (import-input)
  (print (game/1 a b))
  (print (game/2 a b)))
