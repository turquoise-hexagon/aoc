(import
  (chicken io))

(define (parse-game str)
  (apply
    (lambda (a _ b)
      (let*
        ((a (- (char->integer a) (char->integer #\A)))
         (b (- (char->integer b) (char->integer #\X)))
         (c (modulo (+ (- b 1) a) 3)))
        (list a b c)))
    (string->list str)))

(define (import-input)
  (map parse-game (read-lines)))

(define (proc/1 a b c) (+ (* 3 (modulo (+ (- b a) 1) 3)) b 1))
(define (proc/2 a b c) (+ (* 3 (modulo (+ (- c a) 1) 3)) c 1))

(define (solve input proc)
  (apply + (map (lambda (_) (apply proc _)) input)))

(let* ((input (import-input))
       (part/1 (solve input proc/1))
       (part/2 (solve input proc/2)))
  (print part/1) (assert (= part/1 12679))
  (print part/2) (assert (= part/2 14470)))
