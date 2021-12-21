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

(define (rolls)
  (define (roll)
    (set! DIE-STATE (modulo (+ DIE-STATE 1) 100))
    DIE-STATE)
  (+ (roll) (roll) (roll)))

;; part 2 rolls
(define (dirac)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (roll)
        (hash-table-set! acc roll
          (+ (hash-table-ref/default acc roll 0) 1)))
      (map (cut apply + <>) (product '(1 2 3) '(1 2 3) '(1 2 3))))
    (let ((result (hash-table->alist acc)))
      (values (map car result)
              (map cdr result)))))

(define (game/1 a b)
  (define (loop a b score/a score/b count)
    (if (or (>= score/a 1000)
            (>= score/b 1000))
      (* (min score/a score/b) count)
      (let ((a (+ (modulo (+ a -1 (rolls)) 10) 1)))
        (loop b a score/b (+ score/a a) (+ count 3)))))
  (loop a b 0 0 0))

(define (game/2 a b)
  (receive (rolls counts) (dirac)
    (let ((acc (make-hash-table)))
      (define (loop a b score/a score/b)
        (let ((key (list a b score/a score/b)))
          (cond
            ((hash-table-exists? acc key)
             (hash-table-ref     acc key))
            ((>= score/b 21) '(0 1))
            (else
              (let ((res
                      (reverse
                        (fold
                          (lambda (roll count acc)
                            (map + acc
                              (map * `(,count ,count)
                                (let ((a (+ (modulo (+ a -1 roll) 10) 1)))
                                  (loop b a score/b (+ score/a a))))))
                          '(0 0) rolls counts))))
                (hash-table-set! acc key res)
                res)))))
      (apply max (loop a b 0 0)))))

(receive (a b) (import-input)
  (print (game/1 a b))
  (print (game/2 a b)))
