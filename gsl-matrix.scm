(foreign-declare "#include <gsl/gsl_block.h>")
(foreign-declare "#include <gsl/gsl_vector.h>")
(foreign-declare "#include <gsl/gsl_matrix.h>")

;;; BLOCKS
(define-foreign-record-type (gsl_block "gsl_block")
  (unsigned-int size block-size)
  ((c-pointer double) data block-data))

;; Block allocation
(define gsl_block_free
  (foreign-lambda void "gsl_block_free" gsl_block))

(define gsl_block_alloc
  (foreign-lambda gsl_block "gsl_block_alloc" unsigned-int))
(define (gsl_block_alloc_gc n)
  (set-finalizer! (gsl_block_alloc n) gsl_block_free))

(define gsl_block_calloc
  (foreign-lambda gsl_block "gsl_block_calloc" unsigned-int))
(define (gsl_block_calloc_gc n)
  (set-finalizer! (gsl_block_calloc n) gsl_block_free))

;;; VECTORS
(define-foreign-record-type (gsl_vector "gsl_vector")
  (unsigned-int size gsl_vector.size)
  (unsigned-int stride gsl_vector.stride)
  ((c-pointer double) data gsl_vector.data)
  (gsl_block block gsl_vector.block)
  (int owner gsl_vector.owner))

;; Vector allocation
(define gsl_vector_free
  (foreign-lambda void "gsl_vector_free" gsl_vector))

(define gsl_vector_alloc
  (foreign-lambda gsl_vector "gsl_vector_alloc" unsigned-int))
(define (gsl_vector_alloc_gc n)
  (set-finalizer! (gsl_vector_alloc n) gsl_vector_free))

(define gsl_vector_calloc
  (foreign-lambda gsl_vector "gsl_vector_calloc" unsigned-int))
(define (gsl_vector_calloc_gc n)
  (set-finalizer! (gsl_vector_calloc n) gsl_vector_free))

;; Accessing vector elements
(define gsl_vector_get
  (foreign-lambda double "gsl_vector_get"
    (const gsl_vector)
    (const unsigned-int)))

(define gsl_vector_set
  (foreign-lambda void "gsl_vector_set"
    gsl_vector
    (const unsigned-int)
    (const double)))

(define gsl_vector_ptr
  (foreign-lambda (c-pointer double) "gsl_vector_ptr"
    gsl_vector
    unsigned-int))

(define gsl_vector_const_ptr
  (foreign-lambda (c-pointer double) "gsl_vector_const_ptr"
    gsl_vector
    unsigned-int))

;; Initializing vector elements
(define gsl_vector_set_all
  (foreign-lambda void "gsl_vector_set_all" gsl_vector double))

(define gsl_vector_set_zero
  (foreign-lambda void "gsl_vector_set_zero" gsl_vector))

(define gsl_vector_set_basis
  (foreign-lambda void "gsl_vector_set_basis" gsl_vector unsigned-int))

;; Vector views
(define-syntax define-gsl-subview-binding
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((type (conc "gsl_" (second form))))
       `(define ,(string->symbol (conc "gsl_" (second form) "_" (third form)))
          (foreign-lambda*
              ,(string->symbol (conc "gsl_" (fourth form)))
              ,(fifth form)
            ,(conc type " *p0 = malloc(sizeof(" type "));")
            ,(conc "gsl_" (fourth form) "_view p1 = gsl_" (second form) "_" (third form) "("
                   (string-join (map (lambda (a) (symbol->string (second a))) (fifth form)) ",")
                   ");")
            ;; ,(conc "memcpy(p0, &p1." (fourth form) ", sizeof(" type "));")
            ,(conc "memcpy(p0, &p1." (fourth form) ", sizeof(" (conc "gsl_" (fourth form)) "));")
            "C_return(p0);"))))))

(define-gsl-subview-binding vector subvector vector
  ((gsl_vector v)
   (unsigned-int offset)
   (unsigned-int n)))

(define-gsl-subview-binding vector subvector_with_stride vector
  ((gsl_vector v)
   (unsigned-int offset)
   (unsigned-int stride)
   (unsigned-int n)))

;; TODO Complex and array operations skipped here

;; Copying vectors
(define gsl_vector_memcpy
  (foreign-lambda int "gsl_vector_memcpy" gsl_vector gsl_vector))
(define gsl_vector_swap
  (foreign-lambda int "gsl_vector_swap" gsl_vector gsl_vector))

;; Exchanging elements
(define gsl_vector_swap_elements
  (foreign-lambda int "gsl_vector_swap_elements"
    gsl_vector unsigned-int unsigned-int))
(define gsl_vector_reverse
  (foreign-lambda int "gsl_vector_reverse" gsl_vector))

;; Vector operations
(define gsl_vector_add
  (foreign-lambda int "gsl_vector_add" gsl_vector gsl_vector))

(define gsl_vector_sub
  (foreign-lambda int "gsl_vector_sub" gsl_vector gsl_vector))

(define gsl_vector_mul
  (foreign-lambda int "gsl_vector_mul" gsl_vector gsl_vector))

(define gsl_vector_div
  (foreign-lambda int "gsl_vector_div" gsl_vector gsl_vector))

(define gsl_vector_scale
  (foreign-lambda int "gsl_vector_scale" gsl_vector double))

(define gsl_vector_add_const
  (foreign-lambda int "gsl_vector_add_constant" gsl_vector double))

;; Finding max and min elements of vectors
(define gsl_vector_max (foreign-lambda int "gsl_vector_max" gsl_vector))
(define gsl_vector_min (foreign-lambda int "gsl_vector_min" gsl_vector))
;; ;; TODO (define vector-minmax)
(define gsl_vector_max_index (foreign-lambda int "gsl_vector_max_index" gsl_vector))
(define gsl_vector_min_index (foreign-lambda int "gsl_vector_min_index" gsl_vector))
;; TODO (define vector-argminmax)

;; Vector properties
(define gsl_vector_isnull (foreign-lambda bool "gsl_vector_isnull" gsl_vector))
(define gsl_vector_ispos (foreign-lambda bool "gsl_vector_ispos" gsl_vector))
(define gsl_vector_isneg (foreign-lambda bool "gsl_vector_isneg" gsl_vector))
(define gsl_vector_isnonneg (foreign-lambda bool "gsl_vector_isnonneg" gsl_vector))
(define gsl_vector_equal (foreign-lambda bool "gsl_vector_equal" gsl_vector gsl_vector))

;;; MATRICES
(define-foreign-record-type (gsl_matrix "gsl_matrix")
  (unsigned-int size1 gsl_matrix.size1)
  (unsigned-int size2 gsl_matrix.size2)
  (unsigned-int tda gsl_matrix.tda)
  ((c-pointer double) data gsl_matrix.data)
  (gsl_block block gsl_matrix.block)
  (int owner gsl_matrix.owner))

;; ;; Matrix allocation
(define gsl_matrix_free
  (foreign-lambda void "gsl_matrix_free" gsl_matrix))

(define gsl_matrix_alloc
  (foreign-lambda gsl_matrix "gsl_matrix_alloc" unsigned-int unsigned-int))
(define (gsl_matrix_alloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_alloc n1 n2) gsl_matrix_free))

(define gsl_matrix_calloc
  (foreign-lambda gsl_matrix "gsl_matrix_calloc" unsigned-int unsigned-int))
(define (gsl_matrix_calloc_gc n1 n2)
  (set-finalizer! (gsl_matrix_calloc n1 n2) gsl_matrix_free))

;; ;; Accessing matrix elements
(define gsl_matrix_get
  (foreign-lambda double "gsl_matrix_get"
    (const gsl_matrix)
    (const unsigned-int)
    (const unsigned-int)))

(define gsl_matrix_set
  (foreign-lambda void "gsl_matrix_set"
    gsl_matrix
    (const unsigned-int)
    (const unsigned-int)
    double))

(define gsl_matrix_ptr
  (foreign-lambda (c-pointer double) "gsl_matrix_ptr"
    gsl_matrix
    unsigned-int
    unsigned-int))

(define gsL_matrix_const_ptr
  (foreign-lambda (c-pointer double) "gsl_matrix_ptr"
    gsl_matrix
    unsigned-int
    unsigned-int))

;; ;; Initializing matrix elments
(define gsl_matrix_set_all
  (foreign-lambda void "gsl_matrix_set_all"
    gsl_matrix
    double))

(define gsl_matrix_set_zero
  (foreign-lambda void "gsl_matrix_set_zero" gsl_matrix))

(define gsl_matrix-set-identity
  (foreign-lambda void "gsl_matrix_set_identity" gsl_matrix))

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
  (foreign-lambda int "gsl_matrix_memcpy" gsl_matrix gsl_matrix))
(define gsl_matrix_swap
  (foreign-lambda int "gsl_matrix_swap" gsl_matrix gsl_matrix))

;; Copying rows and columns
(define gsl_matrix_get_row
  (foreign-lambda int "gsl_matrix_get_row" gsl_vector gsl_matrix unsigned-int))

(define gsl_matrix_get_col
  (foreign-lambda int "gsl_matrix_get_col" gsl_vector gsl_matrix unsigned-int))

(define gsl_matrix_set_row
  (foreign-lambda int "gsl_matrix_set_row" gsl_matrix unsigned-int gsl_vector))

(define gsl_matrix_set_col
  (foreign-lambda int "gsl_matrix_set_col" gsl_matrix unsigned-int gsl_vector))

;; Exchanging rows and columns
(define gsl_matrix_swap_rows
  (foreign-lambda int "gsl_matrix_swap_rows"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_swap_columns
  (foreign-lambda int "gsl_matrix_swap_columns"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_swap_rowcol
  (foreign-lambda int "gsl_matrix_swap_rowcol"
    gsl_matrix unsigned-int unsigned-int))

(define gsl_matrix_transpose_memcpy
  (foreign-lambda int "gsl_matrix_transpose_memcpy" gsl_matrix gsl_matrix))

(define gsl_matrix_transpose
  (foreign-lambda int "gsl_matrix_transpose" gsl_matrix))

;; Matrix operations
(define gsl_matrix_add
  (foreign-lambda int "gsl_matrix_add" gsl_matrix gsl_matrix))

(define gsl_matrix_sub
  (foreign-lambda int "gsl_matrix_sub" gsl_matrix gsl_matrix))

(define gsl_matrix_mul_elements
  (foreign-lambda int "gsl_matrix_mul_elements" gsl_matrix gsl_matrix))

(define gsl_matrix_div_elements
  (foreign-lambda int "gsl_matrix_div_elements" gsl_matrix gsl_matrix))

(define gsl_matrix_scale
  (foreign-lambda int "gsl_matrix_scale" gsl_matrix double))

(define gsl_matrix_add_const
  (foreign-lambda int "gsl_matrix_add_constant" gsl_matrix double))

;; Finding max and min elements of matrices
(define gsl_matrix_max (foreign-lambda int "gsl_matrix_max" gsl_matrix))
(define gsl_matrix_min (foreign-lambda int "gsl_matrix_min" gsl_matrix))
;; ;; TODO (define matrix-minmax)
(define gsl_matrix_max_index
  (foreign-lambda void "gsl_matrix_max_index"
    gsl_matrix (c-pointer unsigned-long) (c-pointer unsigned-long)))
(define gsl_matrix_min_index
  (foreign-lambda void "gsl_matrix_min_index"
    gsl_matrix (c-pointer unsigned-long) (c-pointer unsigned-long)))
;; TODO (define matrix-argminmax)

;; Matrix properties
(define gsl_matrix_isnull (foreign-lambda bool "gsl_matrix_isnull" gsl_matrix))
(define gsl_matrix_ispos (foreign-lambda bool "gsl_matrix_ispos" gsl_matrix))
(define gsl_matrix_isneg (foreign-lambda bool "gsl_matrix_isneg" gsl_matrix))
(define gsl_matrix_isnonneg (foreign-lambda bool "gsl_matrix_isnonneg" gsl_matrix))
(define gsl_matrix_equal (foreign-lambda bool "gsl_matrix_equal" gsl_matrix gsl_matrix))
