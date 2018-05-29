(foreign-declare "#include <gsl/gsl_matrix.h>")
(foreign-declare "#include <gsl/gsl_complex_math.h>")

(define-foreign-record-type (gsl_vector_complex "gsl_vector_complex")
  (unsigned-int size gsl_vector_complex.size)
  (unsigned-int stride gsl_vector_complex.stride)
  ((c-pointer double) data gsl_vector_complex.data)
  (gsl_block_complex block gsl_vector_complex.block)
  (int owner gsl_vector_complex.owner))

(define-foreign-record-type (gsl_matrix_complex "gsl_matrix_complex")
  (unsigned-int size1 gsl_matrix_complex.size1)
  (unsigned-int size2 gsl_matrix_complex.size2)
  (unsigned-int tda gsl_matrix_complex.tda)
  ((c-pointer double) data gsl_matrix_complex.data)
  (gsl_block_complex block gsl_matrix_complex.block)
  (int owner gsl_matrix_complex.owner))

(define-external (numbers_make_rect (double r) (double i)) scheme-object
  (make-rectangular r i))

;; ;; Matrix allocation
(define gsl_matrix_complex_free
  (foreign-safe-lambda void "gsl_matrix_complex_free" gsl_matrix_complex))

(define gsl_matrix_complex_alloc
  (foreign-safe-lambda gsl_matrix_complex "gsl_matrix_complex_alloc" unsigned-int unsigned-int))
(define (gsl_matrix_complex_alloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_complex_alloc n1 n2) gsl_matrix_complex_free))

(define gsl_matrix_complex_calloc
  (foreign-safe-lambda gsl_matrix_complex "gsl_matrix_complex_calloc" unsigned-int unsigned-int))
(define (gsl_matrix_complex_calloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_complex_calloc n1 n2) gsl_matrix_complex_free))

;; ;; Accessing matrix elements
(define gsl_matrix_complex_get
  (foreign-safe-lambda* scheme-object (((const gsl_matrix_complex) m)
                                       ((const unsigned-int) i)
                                       ((const unsigned-int) j))
    "gsl_complex zout = gsl_matrix_complex_get(m,i,j);"
    "C_return(numbers_make_rect(GSL_REAL(zout),GSL_IMAG(zout)));"))

(define (gsl_matrix_complex_set m i j z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* void ((gsl_matrix_complex m)
                                 ((const unsigned-int) i)
                                 ((const unsigned-int) j)
                                 (double rz) (double iz))
       "gsl_complex zset = gsl_complex_rect(rz,iz);"
       "gsl_matrix_complex_set(m,i,j,zset);")
     m i j rz iz)))

(define gsl_matrix_complex_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_matrix_complex_ptr"
    gsl_matrix_complex
    unsigned-int
    unsigned-int))

(define gsl_matrix_complex_const_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_matrix_complex_ptr"
    gsl_matrix_complex
    unsigned-int
    unsigned-int))

;; ;; Initializing matrix elments
(define (gsl_matrix_complex_set_all m z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* void ((gsl_matrix_complex m)
                                 (double rz) (double iz))
       "gsl_complex zset = gsl_complex_rect(rz,iz);"
       "gsl_matrix_complex_set_all(m,zset);")
     m rz iz)))

(define gsl_matrix_complex_set_zero
  (foreign-safe-lambda void "gsl_matrix_complex_set_zero" gsl_matrix_complex))

(define gsl_matrix_complex_set_identity
  (foreign-safe-lambda void "gsl_matrix_complex_set_identity" gsl_matrix_complex))

(define-syntax define-gsl-matrix-subview-binding
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((type (conc "gsl_" (fourth form))))
       `(define ,(string->symbol (conc "gsl_" (second form) "_" (third form)))
          (foreign-safe-lambda*
              ,(string->symbol (conc "gsl_" (fourth form)))
              ,(fifth form)
            ,(conc type " *p0 = malloc(sizeof(" type "));")
            ,(conc "gsl_" (fourth form) "_view p1 = gsl_" (second form) "_" (third form) "("
                   (string-join (map (lambda (a) (symbol->string (second a))) (fifth form)) ",")
                   ");")
            ,(conc "memcpy(p0, &p1."
                   (case (fourth form)
                     ((vector_complex)
                      "vector")
                     ((matrix_complex)
                      "matrix"))
                   ", sizeof(" (conc "gsl_" (fourth form)) "));")
            "C_return(p0);"))))))

;; ;; Matrix views
(define-gsl-matrix-subview-binding matrix_complex submatrix matrix_complex
  ((gsl_matrix_complex m)
   (unsigned-int k1)
   (unsigned-int k2)
   (unsigned-int n1)
   (unsigned-int n2)))

(define gsl_matrix_complex_submatrix_with_stride
  (foreign-safe-lambda* gsl_matrix_complex ((gsl_matrix_complex m)
                                            (unsigned-int k1)
                                            (unsigned-int k2)
                                            (unsigned-int s1)
                                            (unsigned-int s2)
                                            (unsigned-int n1)
                                            (unsigned-int n2))
    "gsl_matrix_complex *r = gsl_matrix_complex_alloc(n1,n2);"
    "int rm, rr;"
    "for (rm = k1, rr = 0; rr < n1; rr++, rm += s1){"
    "gsl_vector_complex *row = gsl_vector_complex_alloc(m->size2);"
    "gsl_matrix_complex_get_row(row,m,rm);"
    "gsl_vector_complex_view row_with_stride = (s2 > 0) ? gsl_vector_complex_subvector_with_stride(row,k2,s2,n2) : gsl_vector_complex_subvector(row,k2,n2);"
    "gsl_matrix_complex_set_row(r,rr,&row_with_stride.vector);"
    "gsl_vector_complex_free(row);"
    "}"
    "C_return(r);"))
(define-gsl-matrix-subview-binding matrix_complex view_vector matrix_complex
  ((c-pointer v) (unsigned-int n1) (unsigned-int n2)))

;; (define-gsl-matrix-subview-binding matrix view_vector_with_tda matrix
;;   ((c-pointer v) (unsigned-int n1) (unsigned-int n2) (unsigned-int tda)))

;; ;; Creating row and column views
(define-gsl-matrix-subview-binding matrix_complex row vector_complex
  ((c-pointer m) (unsigned-int i)))
(define-gsl-matrix-subview-binding matrix_complex column vector_complex
  ((c-pointer m) (unsigned-int j)))
(define-gsl-matrix-subview-binding matrix_complex subrow vector_complex
  ((c-pointer m) (unsigned-int i) (unsigned-int offset) (unsigned-int n)))
(define-gsl-matrix-subview-binding matrix_complex subcolumn vector_complex
  ((c-pointer m) (unsigned-int j) (unsigned-int offset) (unsigned-int n)))
(define-gsl-matrix-subview-binding matrix_complex diagonal vector_complex
  ((c-pointer m)))
(define-gsl-matrix-subview-binding matrix_complex subdiagonal vector_complex
  ((c-pointer m) (unsigned-int k)))
(define-gsl-matrix-subview-binding matrix_complex superdiagonal vector_complex
  ((c-pointer m) (unsigned-int k)))

;; Copying matrices
(define gsl_matrix_complex_memcpy
  (foreign-safe-lambda int "gsl_matrix_complex_memcpy" gsl_matrix_complex gsl_matrix_complex))
(define gsl_matrix_complex_swap
  (foreign-safe-lambda int "gsl_matrix_complex_swap" gsl_matrix_complex gsl_matrix_complex))

;; Copying rows and columns
(define gsl_matrix_complex_get_row
  (foreign-safe-lambda int "gsl_matrix_complex_get_row" gsl_vector_complex gsl_matrix_complex unsigned-int))

(define gsl_matrix_complex_get_col
  (foreign-safe-lambda int "gsl_matrix_complex_get_col" gsl_vector_complex gsl_matrix_complex unsigned-int))

(define gsl_matrix_complex_set_row
  (foreign-safe-lambda int "gsl_matrix_complex_set_row" gsl_matrix_complex unsigned-int gsl_vector_complex))

(define gsl_matrix_complex_set_col
  (foreign-safe-lambda int "gsl_matrix_complex_set_col" gsl_matrix_complex unsigned-int gsl_vector_complex))

;; Exchanging rows and columns
(define gsl_matrix_complex_swap_rows
  (foreign-safe-lambda int "gsl_matrix_complex_swap_rows"
    gsl_matrix_complex unsigned-int unsigned-int))

(define gsl_matrix_complex_swap_columns
  (foreign-safe-lambda int "gsl_matrix_complex_swap_columns"
    gsl_matrix_complex unsigned-int unsigned-int))

(define gsl_matrix_complex_swap_rowcol
  (foreign-safe-lambda int "gsl_matrix_complex_swap_rowcol"
    gsl_matrix_complex unsigned-int unsigned-int))

(define gsl_matrix_complex_transpose_memcpy
  (foreign-safe-lambda int "gsl_matrix_complex_transpose_memcpy" gsl_matrix_complex gsl_matrix_complex))

(define gsl_matrix_complex_transpose
  (foreign-safe-lambda int "gsl_matrix_complex_transpose" gsl_matrix_complex))

(define gsl_matrix_complex_reverse_rows
  (foreign-safe-lambda* int ((gsl_matrix_complex m))
    "int i;"
    "for (i = 0; i < m->size2; i++){"
    "gsl_vector_complex *col = gsl_vector_complex_alloc(m->size1);"
    "gsl_matrix_complex_get_col(col,m,i);"
    "gsl_vector_complex_reverse(col);"
    "gsl_matrix_complex_set_col(m, i, col);"
    "gsl_vector_complex_free(col);"
    "}"))

(define gsl_matrix_complex_reverse_cols
  (foreign-safe-lambda* int ((gsl_matrix_complex m))
    "int i;"
    "for (i = 0; i < m->size1; i++){"
    "gsl_vector_complex *row = gsl_vector_complex_alloc(m->size2);"
    "gsl_matrix_complex_get_row(row,m,i);"
    "gsl_vector_complex_reverse(row);"
    "gsl_matrix_complex_set_row(m, i, row);"
    "gsl_vector_complex_free(row);"
    "}"))

;; Matrix operations
(define gsl_matrix_complex_add
  (foreign-safe-lambda int "gsl_matrix_complex_add" gsl_matrix_complex gsl_matrix_complex))

(define gsl_matrix_complex_sub
  (foreign-safe-lambda int "gsl_matrix_complex_sub" gsl_matrix_complex gsl_matrix_complex))

(define gsl_matrix_complex_mul_elements
  (foreign-safe-lambda int "gsl_matrix_complex_mul_elements" gsl_matrix_complex gsl_matrix_complex))

(define gsl_matrix_complex_div_elements
  (foreign-safe-lambda int "gsl_matrix_complex_div_elements" gsl_matrix_complex gsl_matrix_complex))

(define (gsl_matrix_complex_scale m z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* int ((gsl_matrix_complex m)
                                (double rz) (double iz))
       "gsl_complex zin = gsl_complex_rect(rz,iz);"
       "gsl_matrix_complex_scale(m,zin);")
     m rz iz)))

(define (gsl_matrix_complex_add_constant m z)
  (let ((rz (real-part z))
        (iz (imag-part z)))
    ((foreign-safe-lambda* int ((gsl_matrix_complex m)
                                (double rz) (double iz))
       "gsl_complex zin = gsl_complex_rect(rz,iz);"
       "gsl_matrix_complex_add_constant(m,zin);")
     m rz iz)))

;; Finding max and min elements of matrices
;; (define gsl_matrix_complex_max (foreign-safe-lambda int "gsl_matrix_complex_max" gsl_matrix_complex))
;; (define gsl_matrix_complex_min (foreign-safe-lambda int "gsl_matrix_complex_min" gsl_matrix_complex))
;; ;; TODO (define matrix-minmax)
;; (define gsl_matrix_complex_max_index
;;   (foreign-safe-lambda void "gsl_matrix_complex_max_index"
;;     gsl_matrix_complex (c-pointer unsigned-long) (c-pointer unsigned-long)))
;; (define gsl_matrix_complex_min_index
;;   (foreign-safe-lambda void "gsl_matrix_complex_min_index"
;;     gsl_matrix_complex (c-pointer unsigned-long) (c-pointer unsigned-long)))
;; TODO (define matrix-argminmax)

;; Matrix properties
(define gsl_matrix_complex_isnull (foreign-safe-lambda bool "gsl_matrix_complex_isnull" gsl_matrix_complex))
(define gsl_matrix_complex_ispos (foreign-safe-lambda bool "gsl_matrix_complex_ispos" gsl_matrix_complex))
(define gsl_matrix_complex_isneg (foreign-safe-lambda bool "gsl_matrix_complex_isneg" gsl_matrix_complex))
(define gsl_matrix_complex_isnonneg (foreign-safe-lambda bool "gsl_matrix_complex_isnonneg" gsl_matrix_complex))
(define gsl_matrix_complex_equal (foreign-safe-lambda bool "gsl_matrix_complex_equal" gsl_matrix_complex gsl_matrix_complex))
