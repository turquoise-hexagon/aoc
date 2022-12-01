(import
  (chicken io)
  (chicken irregex)
  (matchable)
  (srfi 1))

(define (alist->passport lst)
  (let ((res (map
               (lambda (key)
                 (assoc key lst))
               '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))))
    (if (every list? res)
      res
      #f)))

(define (chunk->alist str)
  (map (cut irregex-split ":" <>)
    (irregex-split "[\n ]" str)))

(define (import-input)
  (filter-map alist->passport (map chunk->alist (irregex-split "\n{2}" (read-string #f)))))

(define (is-valid? passport)
  (define (query key) (second (assoc key passport)))
  (and (<= 1920 (string->number (query "byr")) 2002)
       (<= 2010 (string->number (query "iyr")) 2020)
       (<= 2020 (string->number (query "eyr")) 2030)
       (let ((matches (irregex-match "([0-9]+)(cm|in)" (query "hgt"))))
         (if matches
           (match (map (cut irregex-match-substring matches <>) '(1 2))
             ((hgt "cm") (<= 150 (string->number hgt) 193))
             ((hgt "in") (<=  59 (string->number hgt)  76)))
           #f))
       (irregex-match? "#[0-9a-f]{6}"                  (query "hcl"))
       (irregex-match? "(amb|blu|brn|gry|grn|hzl|oth)" (query "ecl"))
       (irregex-match? "[0-9]{9}"                      (query "pid"))))

(define (solve/1 input)
  (length input))

(define (solve/2 input)
  (count is-valid? input))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
