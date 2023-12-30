(import
  (chicken io)
  (chicken irregex))

(define (import-input)
  (read-lines))

(define (proc/1 str)
  (let*
    ((_ str)
     (_ (irregex-replace/all "(^\"|\"$)"           _  ""))
     (_ (irregex-replace/all "\\\\\\\\"            _ "#"))
     (_ (irregex-replace/all "\\\\\""              _ "#"))
     (_ (irregex-replace/all "\\\\x[a-fA-F0-9]{2}" _ "#")))
    (- (string-length str)
       (string-length _))))

(define (proc/2 str)
  (let*
    ((_ str)
     (_ (irregex-replace/all "\\\\" _ "##"))
     (_ (irregex-replace/all "\""   _ "##")))
    (- (string-length _)
       (string-length str)
       -2)))

(define (solve input proc)
  (apply + (map proc input)))

(let ((input (import-input)))
  (let ((part/1 (solve input proc/1)))
    (print part/1) (assert (= part/1 1342)))
  (let ((part/2 (solve input proc/2)))
    (print part/2) (assert (= part/2 2074))))
