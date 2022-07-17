(import
  (chicken io)
  (euler)
  (srfi 1)
  (srfi 69))

(define offsets
  '(( 1  0)
    ( 0  1)
    (-1  0)
    ( 0 -1)))

(define goal/1
  '("#############"
    "#...........#"
    "###A#B#C#D###"
    "  #A#B#C#D#"
    "  #########"))

(define goal/2
  '("#############"
    "#...........#"
    "###A#B#C#D###"
    "  #A#B#C#D#"
    "  #A#B#C#D#"
    "  #A#B#C#D#"
    "  #########"))

(define extension
  '("  #D#C#B#A#"
    "  #D#B#A#C#"))

(define (convert-input input)
  (list->vector
    (map list->vector
      (map string->list
        input))))

(define (extend-input input)
  (receive (part/1 part/2) (split-at input 3)
    (append part/1 extension part/2)))

(define (internalize-input input)
  (let ((tempo (extend-input input)))
    (values (convert-input input) (convert-input goal/1)
            (convert-input tempo) (convert-input goal/2))))

(define (get-amphipods grid)
  (join
    (map
      (lambda (x)
        (let ((line (vector-ref grid x)))
          (filter-map
            (lambda (y)
              (case (vector-ref line y)
                ((#\A #\B #\C #\D)
                 (list x y))
                (else #f)))
            (iota (vector-length line)))))
      (iota (vector-length grid)))))

(define (get-valid-neighbors grid coord)
  (filter
    (lambda (next)
      (char=? (foldl vector-ref grid next) #\.))
    (map
      (lambda (offset)
        (map + offset coord))
      offsets)))

(define (get-all-amphipod-moves grid coord)
  (let ((acc '()))
    (let loop ((coord coord) (path '()))
      (unless (member coord path)
        (let ((path (cons coord path)))
          ;; yikes
          (set! acc (cons path acc))
          (for-each
            (lambda (next)
              (loop next path))
            (get-valid-neighbors grid coord)))))
    (map reverse acc)))

(define (get-all-amphipods-moves grid)
  (join (map
          (lambda (coord)
            (get-all-amphipod-moves grid coord))
          (get-amphipods grid))))

(define (get-room-data grid coord)
  (let ((type (foldl vector-ref grid coord)))
    (values type
      (case type
        ((#\A) 3)
        ((#\B) 5)
        ((#\C) 7)
        ((#\D) 9)))))

(define (valid-room? grid y type)
  (let loop ((x 2) (acc (list 2 y)) (flag #t))
    (let ((tmp (vector-ref (vector-ref grid x) y)))
      (cond ((char=? tmp #\#)
             acc)
            ((char=? tmp #\.)
             (loop (+ x 1) (if flag (list x y) acc) flag))
            ((char=? tmp type)
             (loop (+ x 1) acc #f))
            (else #f)))))

(define (get-amphipods-moves grid)
  (filter
    (lambda (move)
      ;; no staying in place
      (if (= (length move) 1)
        #f
        (let ((a (first move))
              (b (last  move)))
          (let-values (((x/a y/a) (apply values a))
                       ((x/b y/b) (apply values b)))
            (if (= x/a 1)
              ;; no move from a hallway to a hallway
              (if (= x/b 1)
                #f
                (receive (type y) (get-room-data grid a)
                  ;; no invalid move to a room
                  (if (= y/b y)
                    (let ((res (valid-room? grid y type)))
                      (if res
                        (equal? res b)
                        #f))
                    #f)))
              ;; no move to an exit
              (if (= x/b 1)
                (not (or (= y/b 3)
                         (= y/b 5)
                         (= y/b 7)
                         (= y/b 9)))
                (receive (type y) (get-room-data grid a)
                  (if (= y/a y)
                    ;; no moving from a valid room
                    (if (valid-room? grid y type)
                      #f
                      ;; no invalid move to a room
                      (if (= y/b y)
                        (let ((res (valid-room? grid y type)))
                          (if res
                            (equal? res b)
                            #f))
                        #f))
                    ;; no invalid move to a room
                    (if (= y/b y)
                      (let ((res (valid-room? grid y type)))
                        (if res 
                          (equal? res b)
                          #f))
                      #f)))))))))
    (get-all-amphipods-moves grid)))

(define (copy-grid grid)
  (list->vector
    (map list->vector
      (map vector->list
        (vector->list
          grid)))))

(define (do-move grid move)
  (let ((acc (copy-grid grid)))
    (let-values (((x/a y/a) (apply values (first move)))
                 ((x/b y/b) (apply values (last  move))))
      (let ((type (vector-ref (vector-ref acc x/a) y/a)))
        (vector-set! (vector-ref acc x/a) y/a #\.)
        (vector-set! (vector-ref acc x/b) y/b type)))
    acc))

(define (get-move-cost grid move)
  (* (case (foldl vector-ref grid (first move))
       ((#\A) 1)
       ((#\B) 10)
       ((#\C) 100)
       ((#\D) 1000))
     (- (length move) 1)))

(define (comp? a b)
  (< (car a)
     (car b)))

(define (helper! table queue cost grid)
  (foldl
    (lambda (queue move)
      (let ((cost (+ (get-move-cost grid move) cost)) (grid (do-move grid move)))
        (if (> (hash-table-ref/default table grid +inf.0) cost)
          (begin
            (hash-table-set! table grid cost)
            (priority-queue-insert comp? (list cost grid) queue))
          queue)))
    (priority-queue-rest comp? queue) (get-amphipods-moves grid)))

(define (solve from to)
  (let ((acc (make-hash-table)))
    (let loop ((queue (list->priority-queue comp? `((0, from)))))
      (if (priority-queue-empty? queue)
        (hash-table-ref acc to)
        (apply
          (lambda (cost grid)
            (loop (helper! acc queue cost grid)))
          (priority-queue-first queue))))))

(receive (input/1 goal/1 input/2 goal/2) (internalize-input (read-lines))
  (print (solve input/1 goal/1))
  (print (solve input/2 goal/2)))
