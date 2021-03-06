(module csl.vector (vector?
                    list->vector
                    vector->list
                    vector
                    make-vector
                    vector-length
                    vector-map
                    vector-map!
                    vector-ref
                    vector-set!
                    vector-fill!
                    vector-copy
                    vector-copy!
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
          (rename (chicken base) (subvector %subvector))
          (rename (srfi 133) (vector= %vector=)))

  (reexport (only scheme
                  vector?
                  vector
                  make-vector
                  vector-length
                  vector-ref
                  vector-set!
                  vector-fill!))
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

  (define (vector-reverse v)
    (let ((v (vector-copy v)))
      (vector-reverse! v)
      v))

  (define (vector-real-part v)
    (vector-map real-part v))

  (define (vector-imag-part v)
    (vector-map imag-part v))

  (define (vector+ . vectors)
    (apply (cut vector-map + <...>) vectors))

  (define (vector+! . vectors)
    (apply (cut vector-map! + <...>) vectors))

  (define (vector- . vectors)
    (apply (cut vector-map - <...>) vectors))

  (define (vector-! . vectors)
    (apply (cut vector-map! - <...>) vectors))

  (define (vector* . vectors)
    (apply (cut vector-map * <...>) vectors))

  (define (vector*! . vectors)
    (apply (cut vector-map! * <...>) vectors))

  (define (vector/ . vectors)
    (apply (cut vector-map / <...>) vectors))

  (define (vector/! . vectors)
    (apply (cut vector-map! / <...>) vectors))

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

  (define (vector= . vectors)
    (apply (cut %vector= = <...>) vectors))

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
      v))

  (define (subvector v a b #!optional (stride 1))
    (let ((subv (%subvector v a b)))
      (if (> stride 1)
          (let* ((newlen (ceiling (/ (- b a) stride)))
                 (new (make-vector newlen)))
            (do ((i 0 (+ i 1))
                 (j 0 (+ j stride)))
                ((= i newlen) new)
              (vector-set! new i (vector-ref subv j))))
          subv))))

;; Local Variables:
;; compile-command: "csc -vs csl.vector.scm -J"
;; End:
