(module csl-matrix *
  (import chicken scheme foreign foreigners data-structures)
  (use numbers
       csl-vector
       fmt
       srfi-13
       ports)

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

  ;; (define (csl:matrix-ref))

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

  (define (csl:matrix-transpose m)
    (let* ((d (csl:matrix->ptr m))
           (shape (csl:matrix-shape m))
           (rows (car shape))
           (cols (cdr shape))
           (r (gsl_matrix_complex_alloc_gc cols rows)))
      (gsl_matrix_complex_transpose_memcpy r d)
      (csl:ptr->matrix r)))

  (define (csl:matrix-row m n)
    (csl:ptr->vector
     (gsl_matrix_complex_row (csl:matrix->ptr m) n)))

  (define (csl:matrx-column m n)
    (csl:ptr->vector
     (gsl_matrix_complex_column (csl:matrix->ptr m) n)))

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
