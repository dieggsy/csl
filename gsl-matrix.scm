(foreign-declare "#include <gsl/gsl_matrix.h>")

(define-foreign-record-type (gsl_matrix "gsl_matrix")
  (unsigned-int size1 gsl_matrix.size1)
  (unsigned-int size2 gsl_matrix.size2)
  (unsigned-int tda gsl_matrix.tda)
  ((c-pointer double) data gsl_matrix.data)
  (gsl_block block gsl_matrix.block)
  (int owner gsl_matrix.owner))

;; ;; Matrix allocation
(define gsl_matrix_free
  (foreign-safe-lambda void "gsl_matrix_free" gsl_matrix))

(define gsl_matrix_alloc
  (foreign-safe-lambda gsl_matrix "gsl_matrix_alloc" unsigned-int unsigned-int))
(define (gsl_matrix_alloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_alloc n1 n2) gsl_matrix_free))

(define gsl_matrix_calloc
  (foreign-safe-lambda gsl_matrix "gsl_matrix_calloc" unsigned-int unsigned-int))
(define (gsl_matrix_calloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_calloc n1 n2) gsl_matrix_free))

;; ;; Accessing matrix elements
(define gsl_matrix_get
  (foreign-safe-lambda double "gsl_matrix_get"
    (const gsl_matrix)
    (const unsigned-int)
    (const unsigned-int)))

(define gsl_matrix_set
  (foreign-safe-lambda void "gsl_matrix_set"
    gsl_matrix
    (const unsigned-int)
    (const unsigned-int)
    double))

(define gsl_matrix_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_matrix_ptr"
    gsl_matrix
    unsigned-int
    unsigned-int))

(define gsL_matrix_const_ptr
  (foreign-safe-lambda (c-pointer double) "gsl_matrix_ptr"
    gsl_matrix
    unsigned-int
    unsigned-int))

;; ;; Initializing matrix elments
(define gsl_matrix_set_all
  (foreign-safe-lambda void "gsl_matrix_set_all"
    gsl_matrix
    double))

(define gsl_matrix_set_zero
  (foreign-safe-lambda void "gsl_matrix_set_zero" gsl_matrix))

(define gsl_matrix_set_identity
  (foreign-safe-lambda void "gsl_matrix_set_identity" gsl_matrix))

;; ;; Matrix views
(define-gsl-subview-binding matrix submatrix matrix
  ((gsl_matrix m)
   (unsigned-int k1)
   (unsigned-int k2)
   (unsigned-int n1)
   (unsigned-int n2)))

(define-gsl-subview-binding matrix view_array matrix
  ((c-pointer base) (unsigned-int n1) (unsigned-int n2)))

(define-gsl-subview-binding matrix view_array_with_tda matrix
  ((c-pointer base) (unsigned-int n1) (unsigned-int n2) (unsigned-int tda)))

(define-gsl-subview-binding matrix view_vector matrix
  ((c-pointer v) (unsigned-int n1) (unsigned-int n2)))

(define-gsl-subview-binding matrix view_vector_with_tda matrix
  ((c-pointer v) (unsigned-int n1) (unsigned-int n2) (unsigned-int tda)))

;; ;; Creating row and column views
(define-gsl-subview-binding matrix row vector ((c-pointer m) (unsigned-int i)))
(define-gsl-subview-binding matrix column vector
  ((c-pointer m) (unsigned-int j)))
(define-gsl-subview-binding matrix subrow vector
  ((c-pointer m) (unsigned-int i) (unsigned-int offset) (unsigned-int n)))
(define-gsl-subview-binding matrix subcolumn vector
  ((c-pointer m) (unsigned-int j) (unsigned-int offset) (unsigned-int n)))
(define-gsl-subview-binding matrix diagonal vector ((c-pointer m)))
(define-gsl-subview-binding matrix subdiagonal vector
  ((c-pointer m) (unsigned-int k)))
(define-gsl-subview-binding matrix superdiagonal vector
  ((c-pointer m) (unsigned-int k)))

;; Copying matrices
(define gsl_matrix_memcpy
  (foreign-safe-lambda int "gsl_matrix_memcpy" gsl_matrix gsl_matrix))
(define gsl_matrix_swap
  (foreign-safe-lambda int "gsl_matrix_swap" gsl_matrix gsl_matrix))

;; Copying rows and columns
(define gsl_matrix_get_row
  (foreign-safe-lambda int "gsl_matrix_get_row" gsl_vector gsl_matrix unsigned-int))

(define gsl_matrix_get_col
  (foreign-safe-lambda int "gsl_matrix_get_col" gsl_vector gsl_matrix unsigned-int))

(define gsl_matrix_set_row
  (foreign-safe-lambda int "gsl_matrix_set_row" gsl_matrix unsigned-int gsl_vector))

(define gsl_matrix_set_col
  (foreign-safe-lambda int "gsl_matrix_set_col" gsl_matrix unsigned-int gsl_vector))

;; Exchanging rows and columns
(define gsl_matrix_swap_rows
  (foreign-safe-lambda int "gsl_matrix_swap_rows"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_swap_columns
  (foreign-safe-lambda int "gsl_matrix_swap_columns"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_swap_rowcol
  (foreign-safe-lambda int "gsl_matrix_swap_rowcol"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_transpose_memcpy
  (foreign-safe-lambda int "gsl_matrix_transpose_memcpy" gsl_matrix gsl_matrix))

(define gsl_matrix_transpose
  (foreign-safe-lambda int "gsl_matrix_transpose" gsl_matrix))

;; Matrix operations
(define gsl_matrix_add
  (foreign-safe-lambda int "gsl_matrix_add" gsl_matrix gsl_matrix))

(define gsl_matrix_sub
  (foreign-safe-lambda int "gsl_matrix_sub" gsl_matrix gsl_matrix))

(define gsl_matrix_mul_elements
  (foreign-safe-lambda int "gsl_matrix_mul_elements" gsl_matrix gsl_matrix))

(define gsl_matrix_div_elements
  (foreign-safe-lambda int "gsl_matrix_div_elements" gsl_matrix gsl_matrix))

(define gsl_matrix_scale
  (foreign-safe-lambda int "gsl_matrix_scale" gsl_matrix double))

(define gsl_matrix_add_const
  (foreign-safe-lambda int "gsl_matrix_add_constant" gsl_matrix double))

;; Finding max and min elements of matrices
(define gsl_matrix_max (foreign-safe-lambda int "gsl_matrix_max" gsl_matrix))
(define gsl_matrix_min (foreign-safe-lambda int "gsl_matrix_min" gsl_matrix))
;; TODO (define matrix-minmax)
(define gsl_matrix_max_index
  (foreign-safe-lambda void "gsl_matrix_max_index"
    gsl_matrix (c-pointer unsigned-long) (c-pointer unsigned-long)))
(define gsl_matrix_min_index
  (foreign-safe-lambda void "gsl_matrix_min_index"
    gsl_matrix (c-pointer unsigned-long) (c-pointer unsigned-long)))
;; TODO (define matrix-argminmax)

;; Matrix properties
(define gsl_matrix_isnull (foreign-safe-lambda bool "gsl_matrix_isnull" gsl_matrix))
(define gsl_matrix_ispos (foreign-safe-lambda bool "gsl_matrix_ispos" gsl_matrix))
(define gsl_matrix_isneg (foreign-safe-lambda bool "gsl_matrix_isneg" gsl_matrix))
(define gsl_matrix_isnonneg (foreign-safe-lambda bool "gsl_matrix_isnonneg" gsl_matrix))
(define gsl_matrix_equal (foreign-safe-lambda bool "gsl_matrix_equal" gsl_matrix gsl_matrix))
