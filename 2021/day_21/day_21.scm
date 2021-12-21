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
(define dirac
  (map (cut apply + <>)
    (product '(1 2 3)
             '(1 2 3)
             '(1 2 3))))

(define (game/1 a b)
  (let loop ((a a) (b b) (s/a 0) (s/b 0) (r 0))
    (if (or (>= s/a 1000)
            (>= s/b 1000))
      (* (min s/a s/b) r)
      (let ((a (+ (modulo (+ a -1 (die)) 10) 1)))
        (loop b a s/b (+ s/a a) (+ r 3))))))

(define (game/2 a b)
  (let ((acc (make-hash-table)))
    (define (loop a b s/a s/b t)
      (let ((key (list a b s/a s/b t)))
        (if (hash-table-exists? acc key)
          (hash-table-ref acc key)
          (cond
            ((>= s/a 21) '(1 0))
            ((>= s/b 21) '(0 1))
            (else
              (let ((res (apply (cut map + <...>)
                           (map
                             (lambda (roll)
                               (if (= t 0)
                                 (let ((a (+ (modulo (+ a -1 roll) 10) 1)))
                                   (loop a b (+ s/a a) s/b 1))
                                 (let ((b (+ (modulo (+ b -1 roll) 10) 1)))
                                   (loop a b s/a (+ s/b b) 0))))
                             dirac))))
                (hash-table-set! acc key res)
                res))))))
    (apply max (loop a b 0 0 0))))

(receive (a b) (import-input)
  (print (game/1 a b))
  (print (game/2 a b)))
