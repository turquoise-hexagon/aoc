(import
  (chicken io))

(define (parse-game str)
  (apply
    (lambda (a _ b)
      (cons
        (- (char->integer a) (char->integer #\A))
        (- (char->integer b) (char->integer #\X))))
    (string->list str)))

(define (import-input)
  (map parse-game (read-lines)))

(define (proc/1 a b c) (+ (* 3 (modulo (+ (- b a) 1) 3)) b 1))
(define (proc/2 a b c) (+ (* 3 (modulo (+ (- c a) 1) 3)) c 1))

(define (solve input proc)
  (apply +
    (map
      (lambda (game)
        (let* ((a (car game))
               (b (cdr game))
               (c (modulo (+ (- b 1) a) 3)))
          (proc a b c)))
      input)))

(let ((input (import-input)))
  (print (solve input proc/1))
  (print (solve input proc/2)))
