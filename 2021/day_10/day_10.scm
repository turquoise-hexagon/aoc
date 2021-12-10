(import
  (chicken io)
  (chicken sort)
  (srfi 1))

(define openings/scores
  '((#\( . 1)
    (#\[ . 2)
    (#\{ . 3)
    (#\< . 4)))

(define closings/scores
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

(define openings (unzip1 openings/scores))
(define closings (unzip1 closings/scores))

(define matched
  (append (map cons openings closings)
          (map cons closings openings)))

(define (score/1 char)
  (cdr (assoc char closings/scores)))

(define (score/2 lst)
  (foldl
    (lambda (acc char)
      (+ (* acc 5) (cdr (assoc char openings/scores))))
    0 lst))

(define (check-syntax lst)
  (call/cc (lambda (_)
             (foldl
               (lambda (acc char)
                 (if (member char openings)
                   (cons char acc)
                   (if (char=? (cdr (assoc char matched)) (car acc))
                     (cdr acc)
                     (_ char))))
               '() lst))))

(define (import-input)
  (partition list? (map check-syntax (map string->list (read-lines)))))

(define (solve/1 input)
  (apply + (map score/1 input)))

(define (solve/2 input)
  (list-ref (sort (map score/2 input) <) (quotient (length input) 2)))

(receive (lists chars) (import-input)
  (print (solve/1 chars))
  (print (solve/2 lists)))
