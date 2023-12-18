(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (import-input)
  (read-lines))

(define (proc?/1 str)
  (and
    (> (length (irregex-extract "[aeiou]" str)) 2)
    (irregex-match? "^(?!.*(ab|cd|pq|xy)).*" str)
    (irregex-match? ".*([a-z])\\1.*"         str)))

(define (proc?/2 str)
  (and
    (irregex-match? ".*([a-z])[a-z]\\1.*" str)
    (irregex-match? ".*([a-z]{2}).*\\1.*" str)))

(define (solve input proc)
  (count proc input))

(let ((input (import-input)))
  (let ((part/1 (solve input proc?/1)))
    (print part/1) (assert (= part/1 255)))
  (let ((part/2 (solve input proc?/2)))
    (print part/2) (assert (= part/2 55))))
