(import (chicken io)
        (chicken irregex)
        (chicken process-context)
        (chicken string)
        (srfi 1)
        (srfi 69))

(define (import-input path)
  (map
    (lambda (str)
      (let ((hash (make-hash-table)))
        (for-each
          (lambda (lst)
            (hash-table-set! hash (car lst) (cadr lst)))
          (map
            (lambda (str)
              (string-split str ":"))
            (string-split str)))
        hash))
    (irregex-split "\n\n" (read-string #f (open-input-file path)))))

(define (is-valid/1? hash)
  (call/cc
    (lambda (return)
      (for-each
        (lambda (key)
          (unless (hash-table-exists? hash key)
            (return #f)))
        (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))))

(define (is-valid/2? hash)
  (call/cc
    (lambda (return)
      (letrec* ((get
                  (lambda (key)
                    (if (hash-table-exists? hash key)
                        (hash-table-ref     hash key)
                        (return #f))))
                (byr (get "byr"))
                (iyr (get "iyr"))
                (eyr (get "eyr"))
                (hgt (get "hgt"))
                (hcl (get "hcl"))
                (ecl (get "ecl"))
                (pid (get "pid")))
        (and (<= 1920 (string->number byr) 2002)
             (<= 2010 (string->number iyr) 2020)
             (<= 2020 (string->number eyr) 2030)
             (let ((match (irregex-match "([0-9]+)(cm|in)" hgt)))
               (if (irregex-match-data? match)
                   (let ((unt (string->symbol (irregex-match-substring match 2)))
                         (hgt (string->number (irregex-match-substring match 1))))
                     (case unt
                       ((cm) (<= 150 hgt 193))
                       ((in) (<=  59 hgt  76))
                       (else #f)))
                   #f))
             (irregex-match? "#[0-9a-f]{6}"                hcl)
             (irregex-match? "amb|blu|brn|gry|grn|hzl|oth" ecl)
             (irregex-match? "[0-9]{9}"                    pid))))))

(define (solve proc input)
  (display (length (filter proc input)))
  (newline))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve is-valid/1? input)
    (solve is-valid/2? input)))
