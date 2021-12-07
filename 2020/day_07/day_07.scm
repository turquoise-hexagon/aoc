(import
  (chicken io)
  (chicken irregex)
  (srfi 1)
  (srfi 69))

(define (parse-bag str)
  (let ((matches (irregex-match "([0-9]+) (.+)" str)))
    (let ((matches (map (cut irregex-match-substring matches <>) '(1 2))))
      (receive (num color) (apply values matches)
        `(,color . ,(string->number num))))))

(define (parse-rule str)
  (let ((lst (irregex-split " ?(contains?|bags?[,.]?) ?" str)))
    (receive (color . content) (apply values lst)
      `(,color . ,(alist->hash-table (map parse-bag content))))))

(define (import-input)
  (alist->hash-table
    (filter-map
      (lambda (str)
        (if (irregex-match? ".*no other.*" str)
          #f
          (parse-rule str)))
      (read-lines))))

(define (true? bool)
  (equal? bool #t))

(define (solve/1 input main)
  (define (helper color)
    (if (string=? color main)
      #t
      (if (hash-table-exists? input color)
        (let ((content (hash-table-ref input color)))
          (any true? (map helper (hash-table-keys content))))
        #f)))
  (- (count true? (map helper (hash-table-keys input))) 1))

(define (solve/2 input main)
  (define (helper color)
    (if (hash-table-exists? input color)
      (foldl + 1 (hash-table-map (hash-table-ref input color)
                    (lambda (key val)
                      (* val (helper key)))))
      1))
  (- (helper main) 1))

(let ((input (import-input)))
  (print (solve/1 input "shiny gold"))
  (print (solve/2 input "shiny gold")))
