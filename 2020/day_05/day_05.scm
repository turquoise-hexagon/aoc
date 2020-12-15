(import (chicken io)
        (chicken process-context)
        (chicken irregex)
        (srfi 1))

(define (import-input path)
  (map string->list (read-lines (open-input-file path))))

(define (get-seat-id boarding-pass)
  (define (get-seat/h lst)
    (let* ((str (list->string lst))
           (str (irregex-replace/all "[BR]" str "1"))
           (str (irregex-replace/all "[FL]" str "0")))
      (string->number str 2)))
  (+ (* (get-seat/h (take boarding-pass 7)) 8)
     (get-seat/h (drop boarding-pass 7))))

(define (solve/1 ids)
  (print (apply max ids)))

(define (solve/2 ids)
  (let ((alist (map list ids)))
    (print (call/cc
             (lambda (return)
               (for-each
                 (lambda (id)
                   (when (and (not (assoc id alist))
                              (assoc (+ id 1) alist)
                              (assoc (- id 1) alist))
                     (return id)))
                 (iota (+ (* 127 8) 7))))))))

(let ((path (car (command-line-arguments))))
  (let ((ids (map get-seat-id (import-input path))))
    (solve/1 ids)
    (solve/2 ids)))
