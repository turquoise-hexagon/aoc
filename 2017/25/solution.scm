(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define-constant regex/header "\
Begin in state ([A-Z]+).
Perform a diagnostic checksum after ([0-9]+) steps.")

(define-constant regex/rule "\
In state ([A-Z]+):
  If the current value is ([0-9]+):
    - Write the value ([0-9]+).
    - Move one slot to the (left|right).
    - Continue with state ([A-Z]+).
  If the current value is ([0-9]+):
    - Write the value ([0-9]+).
    - Move one slot to the (left|right).
    - Continue with state ([A-Z]+).\n?")

(define (irregex-extract-submatches regex str)
  (let ((match (irregex-match regex str)))
    (map
      (lambda (i)
        (irregex-match-substring match i))
      (iota (irregex-match-num-submatches match) 1))))

(define (parse-rules rules)
  (let ((acc (make-hash-table)))
    (for-each
      (lambda (rule)
        (apply
          (lambda (state cond1 val1 move1 next1 cond2 val2 move2 next2)
            (let
              ((cond1 (string->number cond1)) (val1 (string->number val1)) (move1 (if (string=? move1 "left") -1 +1))
               (cond2 (string->number cond2)) (val2 (string->number val2)) (move2 (if (string=? move2 "left") -1 +1)))
              (hash-table-set! acc state
                (lambda (mem index)
                  (let ((len (vector-length mem)) (val (vector-ref mem index)))
                    (cond
                      ((= val cond1) (vector-set! mem index val1) (list (modulo (+ index move1) len) next1))
                      ((= val cond2) (vector-set! mem index val2) (list (modulo (+ index move2) len) next2))))))))
          (irregex-extract-submatches regex/rule rule)))
      rules)
    acc))

(define (import-input)
  (apply
    (lambda (header . rules)
      (apply
        (lambda (state iterations)
          (values state (string->number iterations) (parse-rules rules)))
        (irregex-extract-submatches regex/header header)))
    (irregex-split "\n\n" (read-string))))

(define (solve state iterations rules)
  (let ((mem (make-vector 10000 0)))
    (let loop ((iteration 0) (index 0) (state state))
      (if (= iteration iterations)
        (foldl + 0 (vector->list mem))
        (apply
          (lambda (index state)
            (loop (+ iteration 1) index state))
          ((hash-table-ref rules state) mem index))))))

(let-values (((state iterations rules) (import-input)))
  (let ((part (solve state iterations rules)))
    (print part) (assert (= part 2725))))
