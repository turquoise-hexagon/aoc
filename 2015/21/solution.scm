(import
  (chicken io)
  (chicken string)
  (euler)
  (srfi 1))

(define-constant weapons
  #(#( 8 4 0)
    #(10 5 0)
    #(25 6 0)
    #(40 7 0)
    #(74 8 0)))

(define-constant armor
  #(#( 13 0 1)
    #( 31 0 2)
    #( 53 0 3)
    #( 75 0 4)
    #(102 0 5)))

(define-constant rings
  #(#( 25 1 0)
    #( 50 2 0)
    #(100 3 0)
    #( 20 0 1)
    #( 40 0 2)
    #( 80 0 3)))

(define-constant options-weapons '(1))
(define-constant options-armor   '(0 1))
(define-constant options-rings   '(0 1 2))

(define-constant player-hp 100)

(define (import-input)
  (filter-map string->number (string-split (read-string) " \n")))

(define (selections)
  (let ((selection-weapons (range (- (vector-length weapons) 1)))
        (selection-armor   (range (- (vector-length armor)   1)))
        (selection-rings   (range (- (vector-length rings)   1))))
    (product
      (join (map (lambda (i) (combinations selection-weapons i)) options-weapons))
      (join (map (lambda (i) (combinations selection-armor i))   options-armor))
      (join (map (lambda (i) (combinations selection-rings i))   options-rings)))))

(define (budget selection)
  (apply +
    (map
      (lambda (type selection)
        (apply +
          (map
            (lambda (i)
              (vector-ref (vector-ref type i) 0))
            selection)))
      (list weapons armor rings) selection)))

(define (stats selection)
  (apply map +
    (join
      (map
        (lambda (type selection)
          (map
            (lambda (i)
              (let ((_ (vector-ref type i)))
                (list
                  (vector-ref _ 1)
                  (vector-ref _ 2))))
            selection))
        (list weapons armor rings) selection))))

(define (fight player boss)
  (apply
    (lambda (          player-damage player-armor
             boss-hp   boss-damage   boss-armor)
      (let loop ((i 0) (player-hp player-hp) (boss-hp boss-hp))
        (cond
          ((<= player-hp 0) #f)
          ((<= boss-hp   0) #t)
          (else
           (if (= (modulo i 2) 0)
             (loop (+ i 1) player-hp (- boss-hp (max 1 (- player-damage boss-armor))))
             (loop (+ i 1) (- player-hp (max 1 (- boss-damage player-armor))) boss-hp))))))
    (append player boss)))

(define (solve/1 input)
  (apply min
    (map budget
      (filter
        (lambda (i)
          (fight (stats i) input))
        (selections)))))

(define (solve/2 input)
  (apply max
    (map budget
      (remove
        (lambda (i)
          (fight (stats i) input))
        (selections)))))

(let ((input (import-input)))
  (let ((part/1 (solve/1 input)))
    (print part/1) (assert (= part/1 91)))
  (let ((part/2 (solve/2 input)))
    (print part/2) (assert (= part/2 158))))
