(import
  (chicken io)
  (chicken irregex)
  (chicken fixnum)
  (srfi 1))

(define-syntax bind
  (syntax-rules ()
    ((_ pat data expr expr* ...)
     (apply (lambda pat expr expr* ...) data))))

(define-constant SIZE 1000)

(define (parse str)
  (bind (instruction . lst) (irregex-extract "(toggle|on|off|[0-9]+)" str)
    (cons (string->symbol instruction) (map string->number lst))))

(define (import-input)
  (map parse (read-lines)))

(define (proc!/1 vec instruction index)
  (vector-set! vec index
    (case instruction
      ((on)     1)
      ((off)    0)
      ((toggle) (fxxor (vector-ref vec index) 1)))))

(define (proc!/2 vec instruction index)
  (vector-set! vec index
    (let ((_ (vector-ref vec index)))
      (case instruction
        ((on)     (fx+ _ 1))
        ((off)    (fxmax 0 (fx- _ 1)))
        ((toggle) (fx+ _ 2))))))

(define (solve input proc)
  (let* ((len (fx* SIZE SIZE)) (mem (make-vector len 0)))
    (for-each
      (lambda (i)
        (bind (instruction a b c d) i
          (do ((i a (fx+ i 1))) ((fx> i c))
            (do ((j b (fx+ j 1))) ((fx> j d))
              (proc mem instruction (fx+ (fx* i SIZE) j))))))
      input)
    (do ((i 0 (fx+ i 1)) (acc 0 (fx+ acc (vector-ref mem i))))
      ((fx= i len) acc))))

(let ((input (import-input)))
  (let ((part/1 (solve input proc!/1)))
    (print part/1) (assert (= part/1 543903)))
  (let ((part/2 (solve input proc!/2)))
    (print part/2) (assert (= part/2 14687245))))
