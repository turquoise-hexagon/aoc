(import (chicken io)
        (chicken process-context)
        (chicken string)
        (chicken irregex)
        (matchable)
        (srfi 1))

(define (parse-rule line)
  (match (string-split line ":")
    ((number content)
     (cons number (list (string-split content "\" "))))))

(define (import-input path)
  (match (map (cut string-split <> "\n") (irregex-split "\n\n" (read-string #f (open-input-file path))))
    ((rules messages)
     (list (map parse-rule rules) messages))))

(define (generate-regex rules n cnt lim)
  (let ((regex
          (fold
            (lambda (a acc)
              (if (string->number a)
                  (let ((cnt (if (string=? a n)
                                   (+ cnt 1)
                                   cnt)))
                    (if (= cnt lim) acc
                        (string-append acc (generate-regex rules a cnt lim))))
                  (string-append acc a)))
            (string) (cadr (assoc n rules)))))
    (string-append "(?:" regex ")")))

(define (solve rules messages n cnt lim)
  (let ((regex (string-append "^" (generate-regex rules n cnt lim) "$")))
    (print (length (filter (cut irregex-match? regex <>) messages)))))

(let ((path (car (command-line-arguments))))
  (match (import-input path)
    ((rules messages)
     (solve rules messages "0" 0 1)
     (let* ((rules (cons (list "8"  (list "42" "|" "42" "8"))            rules))
            (rules (cons (list "11" (list "42" "31" "|" "42" "11" "31")) rules)))
       (solve rules messages "0" 0 5)))))
