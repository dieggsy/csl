(module csl.vector (vector?
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
                    vector-basis!
                    subvector)
  (import scheme
          (chicken module)
          (chicken base)
          (except
           (srfi 133)
           vector=))

  (reexport (only scheme
                  vector?
                  vector
                  make-vector
                  vector-length
                  vector-ref
                  vector-set!
                  vector-fill!))
  (reexport (only (chicken base) subvector))
  (reexport (only srfi-133
                  list->vector
                  vector->list
                  vector-map
                  vector-map!
                  vector-fill!
                  vector-copy
                  vector-copy!
                  vector-swap!
                  vector-reverse!
                  vector-append))

  (define (vector-set v i n)
    (let ((v (vector-copy v)))
      (vector-set! v i n)
      v))

  (define (vector-fill v i n)
    (let ((v (vector-copy v)))
      (vector-fill! v i n)
      v))

  (define (vector-swap v i j)
    (let ((v (vector-copy v)))
      (vector-swap! v i j)
      v))

  (define (vector-reverse v)
    (let ((v (vector-copy v)))
      (vector-reverse! v)
      v))

  (define (vector-real-part v)
    (vector-map real-part v))

  (define (vector-imag-part v)
    (vector-map imag-part v))


  (define (vector+ v1 v2)
    (vector-map + v1 v2))

  (define (vector+! v1 v2)
    (vector-map! + v1 v2))

  (define (vector- v1 v2)
    (vector-map - v1 v2))

  (define (vector-! v1 v2)
    (vector-map! - v1 v2))

  (define (vector* v1 v2)
    (vector-map * v1 v2))

  (define (vector*! v1 v2)
    (vector-map! * v1 v2))

  (define (vector/ v1 v2)
    (vector-map / v1 v2))

  (define (vector/! v1 v2)
    (vector-map! / v1 v2))

  (define (vector-scale v n)
    (vector-map (cut * <> n) v))

  (define (vector-scale! v n)
    (vector-map! (cut * <> n) v))

  (define (vector-add-constant v n)
    (vector-map (cut + <> n) v))

  (define (vector-add-constant! v n)
    (vector-map! (cut + <> n) v))

  (define (vector-zero? v)
    (vector-every zero? v))

  (define (vector-positive? v)
    (vector-every positive? v))

  (define (vector-negative? v)
    (vector-every negative? v))

  (define (vector-nonnegative? v)
    (vector-every (cut > <> 0) v))

  (define (vector= v1 v2)
    (vector-every = v1 v2))

  (define (vector-max v)
    (apply max (vector->list v)))

  (define (vector-min v)
    (apply min (vector->list v)))

  (define (vector-argmax v)
    (let* ((len (vector-length v)))
      (let loop ((index 0)
                 (mindex 0)
                 (max (vector-ref v 0)))
        (if (= index len)
            mindex
            (let ((curr (vector-ref v index)))
              (if (> curr max)
                  (loop (+ index 1) index curr)
                  (loop (+ index 1) mindex max)))))))

  (define (vector-argmin v)
    (let* ((len (vector-length v)))
      (let loop ((index 0)
                 (mindex 0)
                 (min (vector-ref v 0)))
        (if (= index len)
            mindex
            (let ((curr (vector-ref v index)))
              (if (< curr min)
                  (loop (+ index 1) index curr)
                  (loop (+ index 1) mindex min)))))))

  (define (vector-basis! v i)
    (let ((v (make-vector (vector-length v) 0)))
      (vector-set! v i 0)
      v)))
