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

(define (generate-regex rules key lim)
  (define (generate-regex/h key cnt lim)
    (let ((regex (fold
                   (lambda (a acc)
                     (if (string->number a)
                         (let ((cnt (if (string=? a key)
                                        (+ cnt 1)
                                        cnt)))
                           (if (= cnt lim) acc
                               (string-append acc (generate-regex/h a cnt lim))))
                         (string-append acc a)))
                   (string) (cadr (assoc key rules)))))
      (string-append "(?:" regex ")")))
  (string-append "^" (generate-regex/h key 0 lim) "$"))

(define (solve rules messages key lim)
  (let ((regex (generate-regex rules key lim)))
    (print (count (cut irregex-match? regex <>) messages))))

(let ((path (car (command-line-arguments))))
  (match (import-input path)
    ((rules messages)
     (solve rules messages "0" 1)
     (let* ((rules (cons (list "8"  (list "42" "|" "42" "8"))            rules))
            (rules (cons (list "11" (list "42" "31" "|" "42" "11" "31")) rules)))
       (solve rules messages "0" 5)))))
