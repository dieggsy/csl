(module csl-matrix *
  (import chicken scheme foreign foreigners data-structures)
  (use numbers
       csl-vector
       fmt
       srfi-13
       ports
       srfi-1)

  (include "csl-error.scm")
  (include "gsl-block.scm")
  (include "gsl-matrix-complex.scm")

  (define-record-type csl:matrix
    (csl:ptr->matrix data)
    csl:matrix?
    (data csl:matrix->ptr))

  (define (csl:list->matrix lst)
    (let* ((rows (length lst))
           (cols (length (car lst)))
           (m (gsl_matrix_complex_alloc_gc rows cols)))
      (let loop1 ((i 0)
                  (l1 lst))
        (if (= i rows)
            (csl:ptr->matrix m)
            (begin
              (let loop2 ((j 0)
                          (l2 (car l1)))
                (gsl_matrix_complex_set m i j (car l2))
                (if (= j (- cols 1))
                    (void)
                    (loop2 (+ j 1) (cdr l2))))
              (loop1 (+ i 1) (cdr l1)))))))

  (define (csl:matrix->list m)
    (let* ((data (csl:matrix->ptr m))
           (shape (csl:matrix-shape m))
           (rows (car shape))
           (columns (cdr shape)))
      (let loop ((i (- rows 1))
                 (res '()))
        (if (= i -1)
            res
            (loop
             (- i 1)
             (cons
              (let loop ((j (- columns 1))
                         (res '()))
                (if (= j -1)
                    res
                    (loop (- j 1) (cons (gsl_matrix_complex_get data i j) res))))
              res))))))

  (define (csl:matrix . args)
    (csl:list->matrix args))

  (define (csl:make-matrix n1 n2 #!optional fill)
    (let ((m (gsl_matrix_complex_alloc_gc n1 n2)))
      (when fill
        (gsl_matrix_complex_set_all m fill))
      (csl:ptr->matrix m)))

  (define (csl:matrix-shape m)
    (cons
     (gsl_matrix_complex.size1 (csl:matrix->ptr m))
     (gsl_matrix_complex.size2 (csl:matrix->ptr m))))

  (define (csl:matrix-map f . m)
    (let* ((rows (apply min (map (o car csl:matrix-shape) m)))
           (cols (apply min (map (o cdr csl:matrix-shape) m)))
           (d (map csl:matrix->ptr m))
           (r (gsl_matrix_complex_alloc_gc rows cols)))
      (let loop ((i (- rows 1)))
        (if (= i -1)
            (csl:ptr->matrix r)
            (begin
              (let loop ((j (- cols 1)))
                (if (= j -1)
                    #f
                    (begin
                      (gsl_matrix_complex_set r i j
                                              (apply f
                                                     (map (cut gsl_matrix_complex_get <> i j) d)))
                      (loop (- j 1)))))
              (loop (- i 1)))))))

  (define (csl:matrix-map! f . m)
    (let* ((rows (apply min (map (o car csl:matrix-shape) m)))
           (cols (apply min (map (o cdr csl:matrix-shape) m)))
           (d (map csl:matrix->ptr m)))
      (let loop ((i (- rows 1)))
        (if (= i -1)
            (void)
            (begin
              (let loop ((j (- cols 1)))
                (if (= j -1)
                    #f
                    (begin
                      (gsl_matrix_complex_set (car d) i j
                                              (apply f
                                                     (map (cut gsl_matrix_complex_get <> i j) d)))
                      (loop (- j 1)))))
              (loop (- i 1)))))))

  (define (csl:matrix-ref m i #!optional (j '_))
    (define (fix-spec spec max)
      (cond
       ;; No spec
       ((number? spec)
        (if (negative? spec)
            (+ max spec)
            spec))
       ((or (eq? spec '_)
            (null? spec))
        (list 0 max 0))
       ;; Second/third omitted
       ((null? (cdr spec))
        (fix-spec (cons (first spec) `(,max 0)) max))
       ((null? (cddr spec))
        (fix-spec (list (first spec) (second spec) 0) max))
       ;; Third negative, this is the indices of the reversed direction
       ((negative? (third spec))
        (fix-spec (list (if (number? (first spec))
                            (- max (first spec) 1)
                            0)
                        (if (number? (second spec))
                            (- max (second spec) 1)
                            max)
                        (- (third spec)))
                  max))
       ;; First not specified
       ((eq? (first spec) '_)
        (fix-spec (cons 0 (cdr spec)) max))
       ;; First negative
       ((negative? (first spec))
        (fix-spec (cons (+ max (first spec)) (cdr spec)) max))
       ;; First maxed
       ((> (first spec) max)
        (fix-spec (cons max (cdr spec)) max))
       ;; Second not specified
       ((eq? (second spec) '_)
        (fix-spec (list (first spec) max (third spec)) max))
       ;; Second negative
       ((negative? (second spec))
        (fix-spec (list (first spec) (+ max (second spec)) (third spec)) max))
       ;; Scecond maxed
       ((> (second spec) max)
        (fix-spec (list (first spec) (+ max (second spec)) (third spec)) max))
       (else spec)))
    (let* ((m (csl:matrix->ptr m))
           (mcols (gsl_matrix_complex.size2 m))
           (mrows (gsl_matrix_complex.size1 m))
           (fliplr (and (list? j)
                        (= (length j) 3)
                        (negative? (caddr j))))
           (flipud (and (list? i)
                        (= (length i) 3)
                        (negative? (caddr i))))
           (i (fix-spec i mrows))
           (j (fix-spec j mcols)))
      (if (number? i)
          (if (number? j)
              (gsl_matrix_complex_get m i j)
              (let ((vec (csl:ptr->vector (gsl_matrix_complex_row m i))))
                (if (or (not j) (eq? j '_))
                    vec
                    (csl:vector-ref vec j))))
          (if (number? j)
              (let ((vec (csl:ptr->vector (gsl_matrix_complex_column m j))))
                (if (eq? i '_)
                    vec
                    (csl:vector-ref vec i)))
              (let* ((startr (first i))
                     (endr (second i))
                     (stepr (third i))
                     (startc (first j))
                     (endc (second j))
                     (stepc (third j)))
                (cond ((or flipud fliplr)
                       (let ((r (gsl_matrix_complex_alloc_gc mrows mcols)))
                         (gsl_matrix_complex_memcpy r m)
                         (when flipud
                           (gsl_matrix_complex_reverse_rows r))
                         (when fliplr
                           (gsl_matrix_complex_reverse_cols r))
                         (csl:matrix-ref (csl:ptr->matrix r) i j)))
                      ((and (zero? stepr) (zero? stepc))
                       (csl:ptr->matrix
                        (gsl_matrix_complex_submatrix
                         m
                         startr startc
                         (- endr startr)
                         (- endc startc))))
                      (else
                       (csl:ptr->matrix
                        (gsl_matrix_complex_submatrix_with_stride
                         m
                         startr startc
                         stepr stepc
                         (if (zero? stepr)
                             (- endr startr)
                             (inexact->exact (ceiling (/ (- endr startr) stepr))))
                         (if (zero? stepc)
                             (- endc startc)
                             (inexact->exact (ceiling (/ (- endc startc) stepc)))))))))))))

  ;; (define (csl:matrix-set!))

  (define (csl:matrix-fill! m n)
    (gsl_matrix_complex_set_all (csl:matrix->ptr m) n))

  (define (csl:identity-matrix n)
    (let ((m (gsl_matrix_complex_alloc_gc n n)))
      (gsl_matrix_complex_set_identity m)
      (csl:ptr->matrix m)))

  (define (csl:matrix-copy m)
    (let* ((shape (csl:matrix-shape m))
           (c (gsl_matrix_complex_alloc_gc (car shape) (cdr shape))))
      (gsl_matrix_complex_memcpy c (csl:matrix->ptr m))
      (csl:ptr->matrix c)))

  (define (csl:matrix-row-ref m n)
    (let* ((cols (cdr (csl:matrix-shape m)))
           (v (gsl_vector_complex_alloc_gc cols)))
      (gsl_matrix_complex_get_row v (csl:matrix->ptr m) n)
      (csl:ptr->vector v)))

  (define (csl:matrix-column-ref m n)
    (let* ((cols (cdr (csl:matrix-shape m)))
           (v (gsl_vector_complex_alloc_gc cols)))
      (gsl_matrix_complex_column v (csl:matrix->ptr m) n)
      (csl:ptr->vector v)))

  (define (csl:matrix-transpose m)
    (let* ((d (csl:matrix->ptr m))
           (shape (csl:matrix-shape m))
           (rows (car shape))
           (cols (cdr shape))
           (r (gsl_matrix_complex_alloc_gc cols rows)))
      (gsl_matrix_complex_transpose_memcpy r d)
      (csl:ptr->matrix r)))

  (define (csl:matrix-print m out)
    (let* ((cols (csl:matrix->list
                  (csl:matrix-transpose m)))
           (str-cols (map (lambda (x)
                            (string-join
                             (map number->string x)
                             "\n"))
                          cols))
           (fmt-cols (apply append
                            (map (lambda (x)
                                   (list 'right (dsp x) " "))
                                 str-cols)))
           (fmt-str (call-with-output-string
                      (lambda (strout)
                        (fmt strout (apply tabular fmt-cols))))))
      (fprintf out "#,(csl:matrix(~a))"
               (string-join
                (intersperse (map (cut string-drop-right <> 1)
                                  (string-split fmt-str "\n"))
                             (format ")\n~a("
                                     (make-string 13 #\space)))
                ""))))
  ;; ;; (define (matrix-print m out)
  ;; ;;   (let* ((cols ))))

  (define-reader-ctor 'csl:matrix csl:matrix)

  (define-record-printer (csl:matrix m out)
    (let* ((shape (csl:matrix-shape m))
           (rows (car shape))
           (columns (cdr shape)))
      (if (or (> rows 20)
              (> columns 20))
          (fprintf out "#<~ax~a csl:matrix>" rows columns)
          (##sys#with-print-length-limit
           +inf.0
           (lambda ()
             (csl:matrix-print m out)))))))
