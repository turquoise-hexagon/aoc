(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define openings '(#\( #\[ #\{ #\<))
(define closings '(#\) #\] #\} #\>))

(define matched
  (append
    (map cons openings closings)
    (map cons closings openings)))

(define (score/1 char)
  (case char
    ((#\)) 3)
    ((#\]) 57)
    ((#\}) 1197)
    ((#\>) 25137)))

(define (score/2 lst)
  (foldl
    (lambda (acc char)
      (+ (* acc 5)
         (case char
           ((#\() 1)
           ((#\[) 2)
           ((#\{) 3)
           ((#\<) 4))))
    0 lst))

(define (run lst)
  (call/cc (lambda (_)
             (foldl
               (lambda (acc char)
                 (cond ((member char openings)
                        (cons char acc))
                       ((char=? (cdr (assoc char matched)) (car acc))
                        (cdr acc))
                       (else (_ (score/1 char)))))
               '() lst))))

(define (import-input)
  (partition number? (map run (map string->list (read-lines)))))

(define (solve/1 input)
  (apply + input))

(define (solve/2 input)
  (list-ref (sort (map score/2 input) <) (quotient (length input) 2)))

(receive (nums lsts) (import-input)
  (print (solve/1 nums))
  (print (solve/2 lsts)))
