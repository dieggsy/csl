(functor (generic-vector (M (vector?
                             vector-size
                             vector-alloc
                             vector-calloc
                             vector-free!
                             vector-get
                             vector-set!
                             vector-set-all!
                             vector-set-zero!
                             vector-set-basis!
                             vector-fwrite
                             vector-fread
                             vector-fprintf
                             vector-fscanf
                             vector-subvector
                             vector-subvector-with-stride
                             vector-imag
                             vector-real
                             vector-memcpy!
                             vector-swap!
                             vector-swap-elements!
                             vector-reverse!
                             vector-add!
                             vector-sub!
                             vector-mul!
                             vector-div!
                             vector-scale!
                             vector-add-constant!
                             vector-max
                             vector-min
                             vector-max-index
                             vector-min-index
                             vector-isnull?
                             vector-ispos?
                             vector-isneg?
                             vector-isnonneg?
                             vector-equal?)))
    (list->vector
     vector-enable-sharp-syntax
     vector->list
     vector
     make-vector
     vector-length
     vector-map
     vector-map!
     vector-ref
     subvector
     subvector*
     subvector-set!
     subvector-set!*
     subvector-call!
     subvector-call!*
     vector-fill!
     vector-copy
     vector-copy!
     vector-reverse
     vector-reverse!
     vector-append
     vector-real-part
     vector-imag-part
     vector+
     vector-
     vector~*
     vector~/
     vector-scale
     vector-add-constant
     vector-zero?
     vector-positive?
     vector-negative?
     vector-nonnegative?
     vector=
     make-basis-vector)

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
            (only chicken.module reexport)
            (only chicken.base add1 sub1 when cut foldl foldr case-lambda)
            (only chicken.read-syntax set-sharp-read-syntax!)
            (only chicken.port make-concatenated-port call-with-input-string)
            (prefix M gsl:))

  (reexport M)
  (define (list->vector lst #!optional complex)
    (let* ((len (length lst))
           (v (gsl:vector-alloc len)))
      (do ((i 0 (add1 i))
           (lst lst (cdr lst)))
          ((= i len) v)
        (gsl:vector-set! v i (car lst)))))

  (define (vector-enable-sharp-syntax #!optional (on #t))
    (if on
        (set-sharp-read-syntax! #\[
          (lambda (rest-port)
            (call-with-input-string "["
              (lambda (head-port)
                (let* ((port (make-concatenated-port head-port rest-port)))
                  `(list->vector ',(read port)))))))
        (set-sharp-read-syntax! #\[ #f)))

  (define (vector->list v)
    (let* ((len (gsl:vector-size v)))
      (do ((i (sub1 len) (sub1 i))
           (res '() (cons (gsl:vector-get v i) res)))
          ((= i -1) res))))

  (define (vector . args)
    (list->vector args))

  (define (make-vector n #!optional fill)
    (let ((v (gsl:vector-alloc n)))
      (when fill
        (gsl:vector-set-all! v fill))
      v))

  (define vector-length gsl:vector-size)

  (define (vector-map fn . vectors)
    (let* ((len (apply min (map gsl:vector-size vectors)))
           (new (gsl:vector-alloc len)))
      (do ((i 0 (+ i 1)))
          ((= i len) new)
        (gsl:vector-set! new i (apply fn (map (cut gsl:vector-get <> i) vectors))))
      new))

  (define (vector-map! fn . vectors)
    (let* ((len (apply min (map gsl:vector-size vectors))))
      (do ((i 0 (+ i 1)))
          ((= i len))
        (gsl:vector-set! (car vectors) i (apply fn (map (cut gsl:vector-get <> i) vectors))))))

  (define vector-ref gsl:vector-get)

  (define (subvector v #!optional (start 0) (end (vector-length v)) (step 1))
    (gsl:vector-subvector-with-stride v
                                      start
                                      step
                                      (add1 (quotient (- end 1 start ) step))))

  (define (subvector* v #!key (start 0) (end (vector-length v)) (step 1))
    (subvector v start end step))

  (define (subvector-set! v sub #!optional (start 0) (end (vector-length v)) (step 1))
    (gsl:vector-subvector-with-stride-set! v start step (add1 (quotient (- end 1 start) step)) sub))

  (define (subvector-set!* v sub #!key (start 0) (end (vector-length v)) (step 1))
    (subvector-set! v sub start end step))

  (define (subvector-call! f v #!optional (start 0) (end (vector-length v)) (step 1))
    (let* ((sub (subvector v start end step))
           (rep (f sub)))
      (subvector-set! v (if (vector? rep) rep sub) start end step )))

  (define (subvector-call!* f v #!key (start 0) (end (vector-length v)) (step 1))
    (subvector-call! f v start end step))

  (define vector-fill! gsl:vector-set-all!)

  (define (vector-copy v)
    (let* ((new (gsl:vector-alloc (gsl:vector-size vector))))
      (gsl:vector-memcpy! new vector)
      new))

  (define vector-copy! gsl:vector-memcpy!)

  (define (vector-reverse v)
    (let* ((new (gsl:vector-alloc (gsl:vector-size v))))
      (gsl:vector-memcpy! new v)
      (gsl:vector-reverse! new)
      new))

  (define (vector-append . vectors)
    (define (append* ptr1 ptr2)
      (let* ((len1 (gsl:vector-size ptr1))
             (len2 (gsl:vector-size ptr2))
             (newlen (+ len1 len2))
             (new (gsl:vector-alloc newlen)))
        (do ((i 0 (+ i 1)))
            ((= i len1))
          (gsl:vector-set! new i (gsl:vector-get ptr1 i)))
        (do ((i len1 (+ i 1)))
            ((= i newlen))
          (gsl:vector-set! new i (gsl:vector-get ptr2 (- i len1))))
        new))
    (foldl append* (car vectors) (cdr vectors)))

  (define vector-real-part gsl:vector-real)

  (define vector-imag-part gsl:vector-imag)

  (define (vector+ . vectors)
    (let* ((v1 (car vectors))
           (new (gsl:vector-alloc (gsl:vector-size v1))))
      (gsl:vector-memcpy! new v1)
      (for-each (cut gsl:vector-add! new <>) (cdr vectors))
      new))

  (define (vector- . vectors)
    (let* ((v1 (car vectors))
           (new (gsl:vector-alloc (gsl:vector-size v1))))
      (gsl:vector-memcpy! new v1)
      (for-each (cut gsl:vector-sub! new <>) (cdr vectors))
      new))

  (define (vector~* . vectors)
    (let* ((v1 (car vectors))
           (new (gsl:vector-alloc (gsl:vector-size v1))))
      (gsl:vector-memcpy! new v1)
      (for-each (cut gsl:vector-mul! new <>) (cdr vectors))
      new))

  (define (vector~/ . vectors)
    (let* ((v1 (car vectors))
           (new (gsl:vector-alloc (gsl:vector-size v1))))
      (gsl:vector-memcpy! new v1)
      (for-each (cut gsl:vector-div! new <>) (cdr vectors))
      new))

  (define (vector-scale v n)
    (let* ((new (gsl:vector-alloc (gsl:vector-size v))))
      (gsl:vector-memcpy! new v)
      (gsl:vector-scale! new n)
      new))

  (define (vector-add-constant v n)
    (let* ((new (gsl:vector-alloc (gsl:vector-size v))))
      (gsl:vector-memcpy! new v)
      (gsl:vector-add-constant! new n)
      new))

  (define vector-zero? gsl:vector-isnull?)

  (define vector-positive? gsl:vector-ispos?)

  (define vector-negative? gsl:vector-isneg?)

  (define vector-nonnegative? gsl:vector-isnonneg?)

  (define (vector= . vectors)
    (foldr (lambda (x y)
             (and
              (gsl:vector-equal? (car vectors) x)
              y))
           #t
           (cdr vectors)))

  (define (make-basis-vector len n)
    (let ((v (gsl:vector-alloc len)))
      (gsl:vector-set-basis! v n)
      v)))
