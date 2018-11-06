(functor (generic-vector (M (vector-alloc
                             vector-set!
                             vector-size
                             vector-get
                             vector-set-all!
                             vector-memcpy!
                             vector-swap-elements!
                             vector-reverse!
                             vector-isnull?
                             vector-ispositive?
                             vector-isnegative?
                             vector-isnonneg?
                             vector-equal?
                             vector-real
                             vector-imag
                             vector-add!
                             vector-add-constant!
                             vector-sub!
                             vector-mul!
                             vector-scale!
                             vector-div!
                             vector-max
                             vector-min
                             vector-max-index
                             vector-min-index
                             vector-set-basis!
                             vector-subvector-with-stride)))
    (vector?
     ptr->vector
     vector->ptr
     list->vector
     vector->list
     vector
     make-vector
     vector-length
     vector-map
     vector-map!
     vector-ref
     vector-set
     vector-set!
     vector-fill
     vector-fill!
     vector-copy
     vector-copy!
     vector-swap
     vector-swap!
     vector-reverse
     vector-reverse!
     vector-append
     vector-real-part
     vector-imag-part
     vector+
     vector+!
     vector-
     vector-!
     vector*
     vector*!
     vector/
     vector/!
     vector-scale
     vector-scale!
     vector-add-constant
     vector-add-constant!
     vector-zero?
     vector-positive?
     vector-negative?
     vector-nonnegative?
     vector=
     vector-max
     vector-min
     vector-argmax
     vector-argmin
     make-basis-vector
     vector-basis!
     subvector)
    (import (except scheme
                    vector
                    vector?
                    list->vector
                    vector->list
                    make-vector
                    vector-length
                    vector-ref
                    vector-fill!
                    vector-set!)
            (except (chicken base) subvector vector-copy!)
            (prefix M gsl:))

  (define-record-type vector
    (ptr->vector ptr)
    vector?
    (ptr vector->ptr))

  (define (list->vector lst #!optional complex)
    (let* ((len (length lst))
           (v (gsl:vector-alloc (length lst))))
      (do ((i 0 (+ i 1))
           (lst lst (cdr lst)))
          ((= i len) (ptr->vector v))
        (gsl:vector-set! v i (car lst)))))

  (define (vector->list v)
    (let* ((ptr (vector->ptr v))
           (len (gsl:vector-size ptr)))
      (do ((i (- len 1) (- i 1))
           (res '() (cons (gsl:vector-get ptr i) res)))
          ((= i -1) res))))

  (define (vector . args)
    (list->vector args))

  (define (make-vector n #!optional fill)
    (let ((v (gsl:vector-alloc n)))
      (when fill
        (gsl:vector-set-all! v fill))
      (ptr->vector v)))

  (define (vector-length v)
    (gsl:vector-size (vector->ptr v)))

  (define (vector-map fn . vectors)
    (let* ((ptrs (map vector->ptr vectors))
           (len (apply min (map gsl:vector-size ptrs)))
           (new (gsl:vector-alloc len)))
      (do ((i 0 (+ i 1)))
          ((= i len) (ptr->vector new))
        (gsl:vector-set! new i (apply fn (map (cut gsl:vector-get <> i) ptrs))))))

  (define (vector-map! fn . vectors)
    (let* ((ptrs (map vector->ptr vectors))
           (len (apply min (map gsl:vector-size ptrs))))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (gsl:vector-set! (car ptrs) i (apply fn (map (cut gsl:vector-get <> i) ptrs))))))

  (define (vector-ref v i)
    (gsl:vector-get (vector->ptr v) i))

  (define (subvector v a b #!optional (stride 1))
    (ptr->vector (gsl:vector-subvector-with-stride (vector->ptr v)
                                                   a
                                                   stride
                                                   (inexact->exact
                                                    (ceiling (/ (- b a) stride))))))

  (define (vector-set v i n)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-memcpy! new ptr)
      (gsl:vector-set! new i n)
      (ptr->vector new)))

  (define (vector-set! v i n)
    (gsl:vector-set! (vector->ptr v) i n))

  (define (vector-fill v n)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-set-all! new n)
      (ptr->vector new)))

  (define (vector-fill! v n)
    (gsl:vector-set-all! (vector->ptr v) n))

  (define (vector-copy v)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-memcpy! new ptr)
      (ptr->vector new)))

  (define (vector-copy! v1 v2)
    (gsl:vector-memcpy! (vector->ptr v1) (vector->ptr v2)))

  (define (vector-swap v n1 n2)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-swap-elements! new n1 n2)
      (ptr->vector new)))

  (define (vector-swap! v n1 n2)
    (gsl:vector-swap-elements! (vector->ptr v) n1 n2))

  (define (vector-reverse v)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-memcpy! new ptr)
      (gsl:vector-reverse! new)
      (ptr->vector new)))

  (define (vector-reverse! v)
    (gsl:vector-reverse! (vector->ptr v)))

  (define (vector-append v1 v2)
    (let* ((ptr1 (vector->ptr v1))
           (ptr2 (vector->ptr v2))
           (len1 (gsl:vector-size ptr1))
           (len2 (gsl:vector-size ptr2))
           (newlen (+ len1 len2))
           (new (gsl:vector-alloc newlen)))
      (do ((i 0 (+ i 1)))
          ((= i len1))
        (gsl:vector-set! new i (gsl:vector-get ptr1 i)))
      (do ((i len1 (+ i 1)))
          ((= i newlen))
        (gsl:vector-set! new i (gsl:vector-get ptr2 (- i len1))))
      (ptr->vector new)))

  (define (vector-real-part v)
    (ptr->vector (gsl:vector-real (vector->ptr v))))

  (define (vector-imag-part v)
    (ptr->vector (gsl:vector-imag (vector->ptr v))))

  (define (vector+ . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (gsl:vector-alloc (gsl:vector-size ptr1))))
      (gsl:vector-memcpy! new ptr1)
      (for-each (cut gsl:vector-add! new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector+! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut gsl:vector-add! ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector- . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (gsl:vector-alloc (gsl:vector-size ptr1))))
      (gsl:vector-memcpy! new ptr1)
      (for-each (cut gsl:vector-sub! new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector-! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut gsl:vector-sub! ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector* . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (gsl:vector-alloc (gsl:vector-size ptr1))))
      (gsl:vector-memcpy! new ptr1)
      (for-each (cut gsl:vector-mul! new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector*! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut gsl:vector-mul! ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector/ . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (gsl:vector-alloc (gsl:vector-size ptr1))))
      (gsl:vector-memcpy! new ptr1)
      (for-each (cut gsl:vector-div! new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector/! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut gsl:vector-div! ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector-scale v n)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-memcpy! new ptr)
      (gsl:vector-scale! new n)
      (ptr->vector new)))

  (define (vector-scale! v n)
    (gsl:vector-scale! (vector->ptr v) n)
    (void))

  (define (vector-add-constant v n)
    (let* ((ptr (vector->ptr v))
           (new (gsl:vector-alloc (gsl:vector-size ptr))))
      (gsl:vector-memcpy! new ptr)
      (gsl:vector-add-constant! new n)
      (ptr->vector new)))

  (define (vector-add-constant! v n)
    (gsl:vector-add-constant! (vector->ptr v) n)
    (void))

  (define (vector-max v)
    (gsl:vector-max (vector->ptr v)))

  (define (vector-min v)
    (gsl:vector-min (vector->ptr v)))

  (define (vector-argmax v)
    (gsl:vector-max-index (vector->ptr v)))

  (define (vector-argmin v)
    (gsl:vector-min-index (vector->ptr v)))

  (define (vector-zero? v)
    (gsl:vector-isnull? (vector->ptr v)))

  (define (vector-positive? v)
    (gsl:vector-ispositive? (vector->ptr v)))

  (define (vector-negative? v)
    (gsl:vector-isnegative? (vector->ptr v)))

  (define (vector-nonnegative? v)
    (gsl:vector-isnonneg? (vector->ptr v)))

  (define (vector= . vectors)
    (foldr (lambda (x y)
             (and
              (gsl:vector-equal? (car vectors) x)
              y))
           #t
           (map vector->ptr (cdr vectors))))

  (define (make-basis-vector len n)
    (let ((ptr (gsl:vector-alloc len)))
      (gsl:vector-set-basis! ptr n)
      (ptr->vector ptr)))

  (define (vector-basis! v n)
    (gsl:vector-set-basis! (vector->ptr v) n)
    (void)))
