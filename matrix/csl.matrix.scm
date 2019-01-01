(module csl.matrix (matrix?
                    ensure-matrix
                    list->matrix
                    matrix->list
                    make-matrix
                    matrix-rows
                    matrix-columns
                    matrix-map
                    matrix-map!
                    matrix-ref
                    matrix-set!
                    matrix-fill!
                    matrix-row-ref
                    matrix-row-set!
                    matrix-flipud
                    matrix-flipud!
                    matrix-row-append
                    matrix-column-ref
                    matrix-column-set!
                    matrix-fliplr
                    matrix-fliplr!
                    matrix-column-append
                    matrix-copy
                    matrix-copy!
                    matrix-swap!
                    matrix-real-part
                    matrix-imag-part
                    matrix+
                    matrix+!
                    matrix-
                    matrix-!
                    matrix~*
                    matrix~*!
                    matrix~/
                    matrix~/!
                    matrix-scale
                    matrix-scale!
                    matrix-add-constant
                    matrix-add-constant!
                    matrix-zero?
                    matrix-positive?
                    matrix-negative?
                    matrix-nonnegative?
                    matrix=
                    matrix-max
                    matrix-min
                    ;; matrix-argmax
                    ;; matrix-argmin
                    make-identity-matrix
                    matrix-identity!
                    ;; submatrix
                    matrix-row-swap!
                    matrix-column-swap!
                    matrix-row-column-swap!
                    matrix-diagonal
                    matrix-transpose!
                    matrix-transpose)
  (import (except scheme
                  vector-fill!
                  vector->list
                  list->vector)
          (chicken module)
          (except (chicken base) vector-copy!)
          (srfi 133))

  (define (matrix? m)
    (and (vector? m)
         (vector-every vector? m)
         (let ((len (vector-length (vector-ref m 0))))
           (vector-every (lambda (x)
                           (= (vector-length x)
                              len))
                         m))))

  (define (ensure-matrix m)
    (if (matrix? m)
        m
        (error "Not a matrix" m)))

  (define (list->matrix lst)
    (list->vector (map list->vector lst)))

  (define (matrix->list m)
    (vector->list (vector-map vector->list m)))

  (define (make-matrix rows cols #!optional (fill 0))
    (vector-unfold (lambda (x) (make-vector cols fill)) rows))

  (define (matrix-rows m)
    (vector-length m))

  (define (matrix-columns m)
    (vector-length (vector-ref m 0)))

  (define (matrix-map fn . matrices)
    (apply vector-map (cut vector-map fn <...>) matrices))

  (define (matrix-map! fn . matrices)
    (apply vector-map!
           (lambda (#!rest rest)
             (let ((v (car rest)))
               (apply vector-map! fn v (cdr rest))
               v))
           matrices))

  (define (matrix-ref m i j)
    (vector-ref (vector-ref m i) j))

  (define (matrix-set! m i j val)
    (vector-set! (vector-ref m i) j val))

  (define (matrix-fill! m fill)
    (vector-for-each (cut vector-fill! <> fill) m))

  (define (make-identity-matrix n)
    (vector-unfold (lambda (i)
                     (let ((v (make-vector n 0)))
                       (vector-set! v i 1)
                       v))
                   n))

  (define (matrix-identity! m)
    (let ((rows (matrix-rows m)))
      (do ((i 0 (+ i 1)))
          ((= i rows) (void))
        (let ((row (vector-ref m i)))
          (vector-fill! row 0)
          (vector-set! row i 1)))))

  (define (matrix-copy m)
    (vector-map vector-copy m))

  (define (matrix-copy! m1 m2)
    (vector-for-each (cut vector-copy! <> 0 <>) m1 m2))

  (define (matrix-swap! m i0 j0 i1 j1)
    (let ((tmp (matrix-ref m i1 j1)))
      (matrix-set! m i1 j1 (matrix-ref m i0 j0))
      (matrix-set! m i0 j0 tmp)))

  (define (matrix-row-ref m n)
    (vector-ref m n))

  (define (matrix-row-set! m n v)
    (vector-set! m n v))

  (define (matrix-flipud m)
    (vector-reverse-copy m))

  (define (matrix-flipud! m)
    (vector-reverse! m))

  (define (matrix-row-append . matrices)
    (vector-concatenate matrices))

  (define (matrix-row-swap! m i j)
    (let ((tmp (matrix-row-ref m i)))
      (matrix-row-set! m i (matrix-row-ref m j))
      (matrix-row-set! m j tmp)))

  (define (matrix-column-ref m n)
    (vector-map (cut vector-ref <> n) m))

  (define (matrix-column-set! m n v)
    (vector-for-each (cut vector-set! <> n <>) m v))

  (define (matrix-fliplr m)
    (vector-map vector-reverse-copy m))

  (define (matrix-fliplr! m)
    (vector-for-each vector-reverse! m))

  (define (matrix-column-append . matrices)
    (apply (cut vector-map vector-append <...>) matrices))

  (define (matrix-column-swap! m i j)
    (let ((tmp (matrix-column-ref m i)))
      (matrix-column-set! m i (matrix-column-ref m j))
      (matrix-column-set! m j tmp)))

  (define (matrix-transpose m)
    (apply (cut vector-map vector <...>) (vector->list m)))

  (define (matrix-transpose! m)
    (let ((mcopy (vector-map vector-copy m))
          (len (vector-length m)))
      (do ((i 0 (+ i 1)))
          ((= i len) (void))
        (print "i: " i)
        (print "newvec: " (vector-ref mcopy i))
        (vector-map (cut vector-set! <> i <>) m (vector-ref mcopy i)))))

  (define (matrix-row-column-swap! m i j)
    (let ((col (vector-copy (matrix-row-ref m i))))
      (matrix-row-set! m i (matrix-column-ref m j))
      (matrix-column-set! m j col)))

  ;; (define (submatrix m i #!optional i2)
  ;;   (define (fix-index lst max)
  ;;     (let ((empty (null? lst)))
  ;;       (list (or (and lst
  ;;                      (not empty)
  ;;                      (not (null? (cdr lst)))
  ;;                      (car lst))
  ;;                 0)
  ;;             (or (and lst
  ;;                      (not empty)
  ;;                      (not (null? (cdr lst)))
  ;;                      (cadr lst))
  ;;                 (and lst
  ;;                      (not (null? lst))
  ;;                      (car lst))
  ;;                 max)
  ;;             (or (and lst
  ;;                      (not empty)
  ;;                      (not (null? (cdr lst)))
  ;;                      (not (null? (cddr lst)))
  ;;                      (caddr lst))
  ;;                 1))))
  ;;   )

  (define (matrix+ . matrices)
    (apply (cut matrix-map + <...>) matrices))

  (define (matrix+! . matrices)
    (apply (cut matrix-map! + <...>) matrices))

  (define (matrix- . matrices)
    (apply (cut matrix-map - <...>) matrices))

  (define (matrix-! . matrices)
    (apply (cut matrix-map! - <...>) matrices))

  (define (matrix~* . matrices)
    (apply (cut matrix-map * <...>) matrices))

  (define (matrix~*! . matrices)
    (apply (cut matrix-map! * <...>) matrices))

  (define (matrix~/ . matrices)
    (apply (cut matrix-map / <...>) matrices))

  (define (matrix~/! . matrices)
    (apply (cut matrix-map! / <...>) matrices))

  (define (matrix-scale m n)
    (matrix-map (cut * <> n) m))

  (define (matrix-scale! m n)
    (matrix-map! (cut * <> n) m))

  (define (matrix-add-constant m n)
    (matrix-map (cut + <> n) m))

  (define (matrix-add-constant! m n)
    (matrix-map! (cut + <> n) m))

  (define (matrix-real-part m)
    (matrix-map real-part m))

  (define (matrix-imag-part m)
    (matrix-map imag-part m))

  (define (matrix-zero? m)
    (vector-every identity
                  (vector-map (cut vector-every zero? <>)
                              m)))

  (define (matrix-positive? m)
    (vector-every identity
                  (vector-map (cut vector-every positive? <>)
                              m)))

  (define (matrix-negative? m)
    (vector-every identity
                  (vector-map (cut vector-every negative? <>)
                              m)))

  (define (matrix-nonnegative? m)
    (not
     (vector-any identity
                 (vector-map (lambda (v)
                               (vector-any (cut < <> 0) v))
                             m))))

  (define (matrix= . matrices)
    (apply (cut matrix-map = <...>) matrices))

  (define (matrix-max m)
    (vector-fold max 0 (vector-map (cut vector-fold max 0 <>) m)))

  (define (matrix-min m)
    (vector-fold min 0 (vector-map (cut vector-fold min 0 <>) m)))

  ;; TODO: Wrote a manual bounds check here since it returned an empty vector
  ;; on the first out of bounds diagonal. We may want to implement checks like
  ;; this for... every other function.
  (define (matrix-diagonal m #!optional (k 0))
    (let* ((rows (matrix-rows m))
           (cols (matrix-columns m))
           (smallest (min cols rows)))
      (if (>= k 0)
          (begin
            (assert (< k cols) "Index out of bounds.")
            (vector-unfold (lambda (i) (vector-ref (vector-ref m i) (+ i k)))
                           (min (- cols k) smallest)))
          (begin
            (assert (< (- k) rows) "Index out of bounds.")
            (vector-unfold (lambda (i) (vector-ref (vector-ref m (+ i (- k))) i))
                           (min (+ rows k) smallest))))))

  )
