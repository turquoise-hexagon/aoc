(import
  (chicken io)
  (chicken string)
  (euler-syntax)
  (srfi 1))

(define-constant player-hp   50)
(define-constant player-mana 500)

(define-constant shield     7)
(define-constant poison     3)
(define-constant recharge   101)
(define-constant difficulty 1)

(define-constant spells
  '(( 53 4 0 0 0 0)
    ( 73 2 2 0 0 0)
    (113 0 0 6 0 0)
    (173 0 0 0 6 0)
    (229 0 0 0 0 5)))

(define (import-input)
  (filter-map string->number (string-split (read-string) " \n")))

(define-memoized (compute flag spent-mana turn player-hp player-mana boss-hp boss-damage shield-time poison-time recharge-time)
  (let
    ((shield-time   (max 0 (- shield-time   1)))
     (poison-time   (max 0 (- poison-time   1)))
     (recharge-time (max 0 (- recharge-time 1)))
     (player-hp     (- player-hp   (if (and flag turn)     difficulty 0)))
     (boss-hp       (- boss-hp     (if (> poison-time   0) poison     0)))
     (player-mana   (+ player-mana (if (> recharge-time 0) recharge   0)))
     (shield        (if (> shield-time 0) shield 0)))
    (cond
      ((<= player-hp 0) #e1e8)
      ((<= boss-hp   0) spent-mana)
      (else
       (if turn
         (apply min
           (map
             (lambda (spell)
               (apply
                 (lambda (spell-mana spell-damage spell-heal spell-shield-time spell-poison-time spell-recharge-time)
                   (cond
                     ((< player-mana spell-mana) #e1e8)
                     ((and (> shield-time   0) (> spell-shield-time   0)) #e1e8)
                     ((and (> poison-time   0) (> spell-poison-time   0)) #e1e8)
                     ((and (> recharge-time 0) (> spell-recharge-time 0)) #e1e8)
                     (else
                      (compute
                        flag
                        (+ spent-mana  spell-mana)
                        (not turn)
                        (+ player-hp   spell-heal)
                        (- player-mana spell-mana)
                        (- boss-hp     spell-damage)
                        boss-damage
                        (max shield-time   spell-shield-time)
                        (max poison-time   spell-poison-time)
                        (max recharge-time spell-recharge-time)))))
                 spell))
             spells))
         (compute
           flag
           spent-mana
           (not turn)
           (- player-hp (max 1 (- boss-damage shield)))
           player-mana
           boss-hp
           boss-damage
           shield-time
           poison-time
           recharge-time))))))

(define (solve flag boss-hp boss-damage)
  (compute flag 0 #t player-hp player-mana boss-hp boss-damage 0 0 0))

(let ((input (import-input)))
  (let ((part/1 (apply solve #f input)))
    (print part/1))
  (let ((part/2 (apply solve #t input)))
    (print part/2)))
