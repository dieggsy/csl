(define-record-type csl:vector
  (csl:ptr->vector data)
  csl:vector?
  (data csl:vector-ptr))

(define (csl:list->vector lst)
  (let* ((len (length lst))
         (v (gsl_vector_alloc_gc (length lst))))
    (let loop ((i 0)
               (lst lst))
      (gsl_vector_set v i (car lst))
      (if (= i (- len 1))
          (csl:ptr->vector v)
          (loop (+ i 1) (cdr lst))))))

(define (csl:vector->list v)
  (let* ((data (csl:vector-ptr v))
         (len (gsl_vector.size data)))
    (let loop ((i (- len 1))
               (res '()))
      (if (= i -1)
          res
          (loop (- i 1) (cons (gsl_vector_get data i) res))))))

(define (csl:vector . args)
  (csl:list->vector args))

(define (csl:make-vector n #!optional fill)
  (let ((v (gsl_vector_alloc_gc n)))
    (when fill
      (gsl_vector_set_all v fill))
    (csl:ptr->vector v)))

(define (csl:vector-length v)
  (gsl_vector.size (csl:vector-ptr v)))

(define (csl:vector-map f . v)
  (let* ((len (apply min (map csl:vector-length v)))
         (d (map csl:vector-ptr v))
         (r (gsl_vector_alloc_gc len)))
    (let loop ((i (- len 1)))
      (if (= i -1)
          (csl:ptr->vector r)
          (begin
            (gsl_vector_set r i (apply f
                                       (map
                                        (cut gsl_vector_get <> i)
                                        d)))
            (loop (- i 1)))))))

(define (csl:vector-map! f . v)
  (let* ((len (apply min (map csl:vector-length v)))
         (d (map csl:vector-ptr v)))
    (let loop ((i (- len 1)))
      (if (= i -1)
          (void)
          (begin
            (gsl_vector_set (car d) i (apply f (map (cut gsl_vector_get <> i) d)))
            (loop (- i 1)))))))

(define (csl:vector-ref v n)
  (let ((d (csl:vector-ptr v))
        (len (csl:vector-length v)))
    (define (spec-reader i #!optional j step)
      (let* ((i (cond ((eq? i '_)
                       0)
                      ((negative? i)
                       (+ len i))
                      (else i)))
             (i (if (> i len) len i))
             (j (cond ((or (not j)
                           (eq? j '_))
                       len)
                      ((negative? j)
                       (+ len j))
                      (else j)))
             (j (if (> j len) len j)))
        (cond ((and step (negative? step))
               (let ((r (gsl_vector_alloc_gc len)))
                 (gsl_vector_memcpy r d)
                 (gsl_vector_reverse r)
                 (csl:vector-ref (csl:ptr->vector r)
                                 `(,(- len i 1)
                                   ,(- len j 1)
                                   ,(- step)))))
              (step
               (csl:ptr->vector
                (gsl_vector_subvector_with_stride
                 d
                 i
                 step
                 (inexact->exact
                  (ceiling
                   (/ (- j i) step))))))
              (else
               (csl:ptr->vector
                (gsl_vector_subvector d i (- j i)))))))
    (if (list? n)
        (apply spec-reader n)
        (gsl_vector_get
         d
         (if (negative? n) (+ len n) n)))))

(define (csl:vector-set! v n val)
  (let ((d (csl:vector-ptr v))
        (len (csl:vector-length v))
        (val (if (csl:vector? val) (csl:vector-ptr val) val))
        (ref (if (list? val) list-ref gsl_vector_get)))
    (define (spec-reader i #!optional j step)
      (let* ((i (cond ((eq? i '_)
                       0)
                      ((negative? i)
                       (+ len i))
                      (else i)))
             (i (if (> i len) len i))
             (j (cond ((or (not j)
                           (eq? j '_))
                       len)
                      ((negative? j)
                       (+ len j))
                      (else j)))
             (j (if (> j len) len j)))
        (let loop ((k i)
                   (l 0))
          (print "K: " k)
          (print "L: " l)
          (if (or (and (negative? step) (< k (+ j (- step))))
                  (and (positive? step) (> k (- j step))))
              (void)
              (begin
                (gsl_vector_set d k (ref val l))
                (loop (+ k step) (+ l 1)))))))
    (if (list? n)
        (apply spec-reader n)
        (gsl_vector_set d n val))))

(define (csl:vector-fill! v n)
  (gsl_vector_set_all (csl:vector-ptr v) n))

(define (csl:vector-copy v)
  (let ((c (gsl_vector_alloc_gc (csl:vector-length v)))
        (d (csl:vector-ptr v)))
    (gsl_vector_memcpy c d)
    (csl:ptr->vector c)))

(define (csl:vector-swap! v n1 n2)
  (gsl_vector_swap_elements (csl:vector-ptr v) n1 n2))

(define (csl:vector-reverse v)
  (let ((r (gsl_vector_alloc_gc (csl:vector-length v))))
    (gsl_vector_memcpy r (csl:vector-ptr v))
    (gsl_vector_reverse r)
    (csl:ptr->vector r)))

(define (csl:vector-reverse! v)
  (gsl_vector_reverse (csl:vector-ptr v)))

(define (csl:vector-add v1 v2)
  (let ((c (csl:vector-copy v1)))
    (gsl_vector_add (csl:vector-ptr c) (csl:vector-ptr v2))
    c))

(define (csl:vector-add! v1 v2)
  (gsl_vector_add (csl:vector-ptr v1) (csl:vector-ptr v2))
  (void))

(define (csl:vector-sub v1 v2)
  (let ((c (csl:vector-copy v1)))
    (gsl_vector_sub (csl:vector-ptr c) (csl:vector-ptr v2))
    c))

(define (csl:vector-sub! v1 v2)
  (gsl_vector_sub (csl:vector-ptr v1) (csl:vector-ptr v2))
  (void))

(define (csl:vector-mul v1 v2)
  (let ((c (csl:vector-copy v1)))
    (gsl_vector_mul (csl:vector-ptr c) (csl:vector-ptr v2))
    c))

(define (csl:vector-mul! v1 v2)
  (gsl_vector_mul (csl:vector-ptr v1) (csl:vector-ptr v2))
  (void))

(define (csl:vector-div v1 v2)
  (let ((c (csl:vector-copy v1)))
    (gsl_vector_div (csl:vector-ptr c) (csl:vector-ptr v2))
    c))

(define (csl:vector-div! v1 v2)
  (gsl_vector_div (csl:vector-ptr v1) (csl:vector-ptr v2))
  (void))


(define (csl:vector-scale v n)
  (let ((c (csl:vector-copy v)))
    (gsl_vector_scale (csl:vector-ptr c) n)
    c))

(define (csl:vector-scale! v n)
  (gsl_vector_scale (csl:vector-ptr v) (csl:vector-ptr n))
  (void))

(define (csl:vector-add-const v n)
  (let ((c (csl:vector-copy v)))
    (gsl_vector_add_const (csl:vector-ptr c) n)
    c))

(define (csl:vector-add-const! v n)
  (gsl_vector_add_const (csl:vector-ptr v) (csl:vector-ptr n))
  (void))

(define (csl:vector-max v)
  (gsl_vector_max (csl:vector-ptr v)))

(define (csl:vector-min v)
  (gsl_vector_min (csl:vector-ptr v)))

(define (csl:vector-minmax v)
  (gsl_vector_minmax (csl:vector-ptr v)))

(define (csl:vector-argmax v)
  (gsl_vector_max_index (csl:vector-ptr v)))

(define (csl:vector-argmin v)
  (gsl_vector_min_index (csl:vector-ptr v)))

(define (csl:vector-argminmax v)
  (gsl_vector_minmax_index (csl:vector-ptr v)))

(define (csl:vector-zero? v)
  (gsl_vector_isnull (csl:vector-ptr v)))

(define (csl:vector-positive? v)
  (gsl_vector_ispos (csl:vector-ptr v)))

(define (csl:vector-negative? v)
  (gsl_vector_isneg (csl:vector-ptr v)))

(define (csl:vector-nonnegative? v)
  (gsl_vector_isnonneg (csl:vector-ptr v)))

(define (csl:vector= v1 v2)
  (gsl_vector_equal (csl:vector-ptr v1) (csl:vector-ptr v2)))

(define-reader-ctor 'csl:vector csl:vector)

(define-record-printer (csl:vector v out)
  (let ((size (csl:vector-length v)))
    (if (> size 20)
        (fprintf out "#<~a csl:vector>" size)
        (let ((str (format "~a" (csl:vector->list v))))
          (fprintf out "#,(csl:vector ~a)"
                   (substring str 1 (- (string-length str) 1)))))))
