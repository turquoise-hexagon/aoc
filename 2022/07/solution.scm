(import
  (chicken io)
  (chicken irregex)
  (matchable)
  (srfi 1)
  (srfi 69))

(define (run-commands commands)
  (let ((acc (make-hash-table)))
    (let loop ((lst commands) (path '()))
      (if (null? lst)
        acc
        (loop (cdr lst)
          (match (car lst)
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
             path)))))))

(define (processed-tree? tree)
  (every
    (lambda (path)
      (number? (hash-table-ref tree path)))
    (hash-table-keys tree)))

(define (processed-contents? tree contents)
  (every
    (lambda (content)
      (apply
        (lambda (size path)
          (or (not (string=? size "dir"))
              (let ((_ (hash-table-ref tree path)))
                (number? _))))
        content))
    contents))

(define (contents-size tree contents)
  (apply +
    (map
      (lambda (content)
        (apply
          (lambda (size path)
            (if (string=? size "dir")
              (hash-table-ref tree path)
              (string->number size)))
          content))
      contents)))

(define (process-commands commands)
  (let ((tree (run-commands commands)))
    (let loop ()
      (hash-table-for-each tree
        (lambda (path contents)
          (when (list? contents)
            (when (processed-contents? tree contents)
              (hash-table-set! tree path
                (contents-size tree contents))))))
      (if (processed-tree? tree)
        tree
        (loop)))))

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
        (>= 100000 size))
      (hash-table-values input))))

(define (solve/2 input)
  (let ((free-space (- 70000000 (hash-table-ref input '("/")))))
    (apply min
      (filter
        (lambda (size)
          (>= (+ free-space size) 30000000))
        (hash-table-values input)))))

(let ((input (import-input)))
  (print (solve/1 input))
  (print (solve/2 input)))
