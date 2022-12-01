(import
  (chicken io)
  (chicken irregex)
  (srfi 1))

(define (parse-rule str)
  (receive (number content) (apply values (irregex-split ":" str))
    (list number (irregex-split "[\" ]" content))))

(define (import-input)
  (let ((lst (map (cut irregex-split "\n" <>) (irregex-split "\n{2}" (read-string #f)))))
    (receive (rules messages) (apply values lst)
      (values (map parse-rule rules) messages))))

(define (generate-regex rules key lim)
  (define (loop key cnt lim)
    (let ((regex
            (foldl
              (lambda (acc a)
                (if (string->number a)
                  (let ((cnt (if (string=? a key)
                               (+ cnt 1)
                               cnt)))
                    (if (= cnt lim)
                      acc
                      (string-append acc (loop a cnt lim))))
                  (string-append acc a)))
              (string) (cadr (assoc key rules)))))
      (string-append "(?:" regex ")")))
  (string-append "^" (loop key 0 lim) "$"))

(define (solve rules messages key lim)
  (let ((regex (generate-regex rules key lim)))
    (count (cut irregex-match? regex <>) messages)))

(receive (rules messages) (import-input)
   (print (solve rules messages "0" 1))
   (let* ((rules (cons '("8"  ("42" "|" "42" "8"))            rules))
          (rules (cons '("11" ("42" "31" "|" "42" "11" "31")) rules)))
     (print (solve rules messages "0" 5))))
