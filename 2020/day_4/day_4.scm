(import (chicken io)
        (chicken irregex)
        (chicken process-context)
        (chicken string)
        (matchable)
        (srfi 1))

(define-record passport byr iyr eyr hgt hcl ecl pid)

(define (import-input path)
  (filter (lambda (x) x) (map alist->passport (map chunk->alist (irregex-split "\n\n" (read-string #f (open-input-file path)))))))

(define (chunk->alist chunk)
  (map
    (lambda (str)
      (match (string-split str ":")
        ((key value) (cons key value))))
    (string-split chunk)))

(define (alist->passport alist)
  (call/cc
    (lambda (return)
      (define (alist->passport/h key)
        (let ((query (assoc key alist)))
          (if query (cdr query) (return #f))))
      (make-passport
        (string->number (alist->passport/h "byr"))
        (string->number (alist->passport/h "iyr"))
        (string->number (alist->passport/h "eyr"))
        (alist->passport/h "hgt")
        (alist->passport/h "hcl")
        (alist->passport/h "ecl")
        (alist->passport/h "pid")))))

(define (solve/1 input)
  (print (length input)))

(define (is-valid? input)
  (match input
    (($ passport byr iyr eyr hgt hcl ecl pid)
     (and (<= 1920 byr 2002)
          (<= 2010 iyr 2020)
          (<= 2020 eyr 2030)
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
          (irregex-match? "[0-9]{9}"                    pid)))))

(define (solve/2 input)
  (print (length (filter is-valid? input))))

(let ((path (car (command-line-arguments))))
  (let ((input (import-input path)))
    (solve/1 input)
    (solve/2 input)))
