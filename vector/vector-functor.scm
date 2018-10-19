(functor (generic-vector (M (valloc
                             vset!
                             vlength
                             vref
                             vfill!
                             vcopy!
                             vswap!
                             vreverse!
                             vnull?
                             vpositive?
                             vnegative?
                             vnonnegative?
                             vequal?
                             vreal
                             vimag
                             v+
                             v+c
                             v-
                             v*
                             v/
                             vmax
                             vmin
                             vargmax
                             vargmin
                             vbasis!
                             vsubvector)))
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
            M)

  (define-record-type vector
    (ptr->vector ptr)
    vector?
    (ptr vector->ptr))

  (define (list->vector lst #!optional complex)
    (let* ((len (length lst))
           (v (valloc (length lst))))
      (do ((i 0 (+ i 1))
           (lst lst (cdr lst)))
          ((= i len) (ptr->vector v))
        (vset! v i (car lst)))))

  (define (vector->list v)
    (let* ((ptr (vector->ptr v))
           (len (vlength ptr)))
      (do ((i (- len 1) (- i 1))
           (res '() (cons (vref ptr i) res)))
          ((= i -1) res))))

  (define (vector . args)
    (list->vector args))

  (define (make-vector n #!optional fill)
    (let ((v (valloc n)))
      (when fill
        (vfill! v fill))
      (ptr->vector v)))

  (define (vector-length v)
    (vlength (vector->ptr v)))

  (define (vector-map fn . vectors)
    (let* ((ptrs (map vector->ptr vectors))
           (len (apply min (map vlength ptrs)))
           (new (valloc len)))
      (do ((i 0 (+ i 1)))
          ((= i len) (ptr->vector new))
        (vset! new i (apply fn (map (cut vref <> i) ptrs))))))

  (define (vector-map! fn . vectors)
    (let* ((ptrs (map vector->ptr vectors))
           (len (apply min (map vlength ptrs))))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (vset! (car ptrs) i (apply fn (map (cut vref <> i) ptrs))))))

  (define (vector-ref v i)
    (vref (vector->ptr v) i))

  (define (subvector v a b #!optional (stride 1))
    (ptr->vector (vsubvector (vector->ptr v)
                             a
                             stride
                             (inexact->exact
                              (ceiling (/ (- b a) stride))))))

  (define (vector-set v i n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (vset! new i n)
      (ptr->vector new)))

  (define (vector-set! v i n)
    (vset! (vector->ptr v) i n))

  (define (vector-fill v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vfill! new n)
      (ptr->vector new)))

  (define (vector-fill! v n)
    (vfill! (vector->ptr v) n))

  (define (vector-copy v)
    (let ((ptr (vector->ptr v))
          (new (valloc (vlength v))))
      (vcopy! new ptr)
      (ptr->vector new)))

  (define (vector-copy! v1 v2)
    (vcopy! (vector->ptr v1) (vector->ptr v2)))

  (define (vector-swap v n1 n2)
    (let* ((ptr (vector->ptr v))
           (new ((valloc (vlength ptr)))))
      (vswap! new n1 n2)
      (ptr->vector new)))

  (define (vector-swap! v n1 n2)
    (vswap! (vector->ptr v) n1 n2))

  (define (vector-reverse v)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (vreverse! new)
      (ptr->vector new)))

  (define (vector-reverse! v)
    (vreverse! (vector->ptr v)))

  (define (vector-append v1 v2)
    (let* ((ptr1 (vector->ptr v1))
           (ptr2 (vector->ptr v2))
           (len1 (vlength ptr1))
           (len2 (vlength ptr2))
           (newlen (+ len1 len2))
           (new (valloc newlen)))
      (do ((i 0 (+ i 1)))
          ((= i len1))
        (vset! new i (vref ptr1 i)))
      (do ((i len1 (+ i 1)))
          ((= i newlen))
        (vset! new i (vref ptr2 (- i len1))))
      (ptr->vector new)))

  (define (vector-real-part v)
    (ptr->vector (vreal (vector->ptr v))))

  (define (vector-imag-part v)
    (ptr->vector (vimag (vector->ptr v))))

  (define (vector+ . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (valloc (vlength ptr1))))
      (vcopy! new ptr1)
      (for-each (cut v+ new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector+! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut v+ ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector- . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (valloc (vlength ptr1))))
      (vcopy! new ptr1)
      (for-each (cut v- new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector-! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut v- ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector* . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (valloc (vlength ptr1))))
      (vcopy! new ptr1)
      (for-each (cut v* new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector*! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut v* ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector/ . vectors)
    (let* ((ptr1 (vector->ptr (car vectors)))
           (new (valloc (vlength ptr1))))
      (vcopy! new ptr1)
      (for-each (cut v/ new <>) (map vector->ptr (cdr vectors)))
      (ptr->vector new)))

  (define (vector/! . vectors)
    (let* ((ptr1 (vector->ptr (car vectors))))
      (for-each (cut v/ ptr1 <>) (map vector->ptr (cdr vectors)))))

  (define (vector-scale v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (v*c new n)
      (ptr->vector new)))

  (define (vector-scale! v n)
    (v*c (vector->ptr v) n)
    (void))

  (define (vector-add-constant v n)
    (let* ((ptr (vector->ptr v))
           (new (valloc (vlength ptr))))
      (vcopy! new ptr)
      (v+c new n)
      (ptr->vector new)))

  (define (vector-add-constant! v n)
    (v+c (vector->ptr v) n)
    (void))

  (define (vector-max v)
    (vmax (vector->ptr v)))

  (define (vector-min v)
    (vmin (vector->ptr v)))

  (define (vector-argmax v)
    (vargmax (vector->ptr v)))

  (define (vector-argmin v)
    (vargmin (vector->ptr v)))

  (define (vector-zero? v)
    (vnull? (vector->ptr v)))

  (define (vector-positive? v)
    (vpositive? (vector->ptr v)))

  (define (vector-negative? v)
    (vnegative? (vector->ptr v)))

  (define (vector-nonnegative? v)
    (vnonnegative? (vector->ptr v)))

  (define (vector= . vectors)
    (foldr (lambda (x y)
             (and
              (vequal? (car vectors) x)
              y))
           #t
           (map vector->ptr (cdr vectors))))

  (define (make-basis-vector len n)
    (let ((ptr (valloc len)))
      (vbasis! ptr n)
      (ptr->vector ptr)))

  (define (vector-basis! v n)
    (vbasis! (vector->ptr v) n)
    (void)))
