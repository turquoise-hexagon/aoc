(import
  (chicken io)
  (chicken irregex)
  (matchable)
  (srfi 1)
  (srfi 69))

(define (run-commands commands)
  (let ((acc (make-hash-table)))
    (foldl
      (lambda (path command)
        (match command
          ((("cd" "..")) (cdr path))
          ((("cd" name)) (cons name path))
          ((("ls") . contents)
           (for-each
             (lambda (content)
               (apply
                 (lambda (size name)
                   (hash-table-update!/default acc path
                     (lambda (_)
                       (cons (list size (cons name path)) _))
                     '()))
                 content))
             contents)
           path)))
      '() commands)
    acc))

(define (process-commands commands)
  (let ((acc (run-commands commands)))
    (let loop ((path '("/")))
      (let ((_
              (apply +
                (map
                  (lambda (content)
                    (apply
                      (lambda (size path)
                        (if (string=? size "dir") (loop path)
                          (string->number size)))
                      content))
                  (hash-table-ref acc path)))))
        (hash-table-set! acc path _)
        _))
    acc))

(define (import-input)
  (process-commands
    (map
      (lambda (chunk)
        (map
          (lambda (line)
            (irregex-split " " line))
          (irregex-split "\n" chunk)))
      (irregex-split "\\$ " (read-string #f)))))

(define (solve/1 input)
  (apply +
    (filter
      (lambda (size)
        (>= #e1e5 size))
      (hash-table-values input))))

(define (solve/2 input)
  (let ((free-space (- #e7e7 (hash-table-ref input '("/")))))
    (apply min
      (filter
        (lambda (size)
          (>= (+ free-space size) #e3e7))
        (hash-table-values input)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
