;; High level wrapper
(define-record-type csl:vector
  (csl:make-vector-record data)
  csl:vector?
  (data csl:vector-data))

(define (csl:vector-length v)
  (gsl_vector.size (csl:vector-data v)))

(define (csl:vector-map f v)
  (let* ((len (csl:vector-length v))
         (d (csl:vector-data v))
         (r (gsl_vector_alloc_gc len)))
    (let loop ((i (- len 1)))
      (if (= i -1)
          (csl:make-vector-record r)
          (begin
            (gsl_vector_set r i (f (gsl_vector_get d i)))
            (loop (- i 1)))))))

(define (csl:vector-map! f v)
  (let* ((len (csl:vector-length v))
         (d (csl:vector-data v)))
    (let loop ((i (- len 1)))
      (if (= i -1)
          (void)
          (begin
            (gsl_vector_set d i (f (gsl_vector_get d i)))
            (loop (- i 1)))))))

(define (csl:vector->list v)
  (let* ((data (csl:vector-data v))
         (len (gsl_vector.size data)))
    (let loop ((i (- len 1))
               (res '()))
      (if (= i -1)
          res
          (loop (- i 1) (cons (gsl_vector_get data i) res))))))

(define (csl:list->vector lst)
  (let* ((len (length lst))
         (v (gsl_vector_alloc_gc (length lst))))
    (let loop ((i 0)
               (lst lst))
      (gsl_vector_set v i (car lst))
      (if (= i (- len 1))
          (csl:make-vector-record v)
          (loop (+ i 1) (cdr lst))))))

(define (csl:vector . args)
  (csl:list->vector args))

(define (csl:make-vector n #!optional fill)
  (let ((v (gsl_vector_alloc_gc n)))
    (when fill
      (gsl_vector_set_all v fill))
    (csl:make-vector-record v)))

(define (csl:vector-ref v n)
  (let ((vpointer (csl:vector-data v)))
    (define (spec-reader i #!optional j step)
      (let* ((len (csl:vector-length v))
             (i (cond ((eq? i '_)
                       0)
                      ((negative? i)
                       (- len i))
                      (else i)))
             (j (cond ((eq? i '_)
                       len)
                      ((negative? i)
                       (- len i))
                      (else i))))
        (cond (step
               (csl:make-vector-record
                (gsl_vector_subvector_with_stride vpointer i step (- j i))))
              (else
               (csl:make-vector-record
                (gsl_vector_subvector vpointer i (- j i)))))))
    (if (list? n)
        (apply spec-reader n)
        (gsl_vector_get vpointer n))))

(define-reader-ctor 'csl:vector csl:vector)

(define-record-printer (csl:vector v out)
  (let ((size (csl:vector-length v)))
    (if (> size 20)
        (fprintf out "#<~a csl:vector>" size)
        (let ((str (format "~a" (csl:vector->list v))))
          (fprintf out "#,(csl:vector ~a)"
                   (substring str 1 (- (string-length str) 1)))))))

;; (define-record-type matrix
;;   (%make-matrix data)
;;   matrix?
;;   (data matrix-data))

;; (define (matrix-shape m)
;;   (cons
;;    (matrix-size1 (matrix-data m))
;;    (matrix-size2 (matrix-data m))))

;; (define (matrix->list m)
;;   (let* ((data (matrix-data m))
;;          (shape (matrix-shape m))
;;          (rows (car shape))
;;          (columns (cdr shape)))
;;     (let loop ((i (- rows 1))
;;                (res '()))
;;       (if (= i -1)
;;           res
;;           (loop
;;            (- i 1)
;;            (cons
;;             (let loop ((j (- columns 1))
;;                        (res '()))
;;               (if (= j -1)
;;                   res
;;                   (loop (- j 1) (cons (matrix-get data i j) res))))
;;             res)))
;;       )))


;; ;; (define (matrix-print m out)
;; ;;   (let* ((cols ))))

;; ;; (define-record-printer (matrix m out)
;; ;;   (let* ((shape (matrix-shape m))
;; ;;          (rows (car shape))
;; ;;          (columns (cdr shape)))
;; ;;     (if (or (> rows 20)
;; ;;             (> columns 20))
;; ;;         (fprintf out "#<~ax~a matrix>" rows columns)
;; ;;         (##sys#with-print-length-limit
;; ;;          +inf.0
;; ;;          (lambda ()
;; ;;            (matrix-print m out))))))
;; (define (make-matrix n1 n2 #!optional fill)
;;   (let ((m (matrix-alloc n1 n2)))
;;     (when fill
;;       (matrix-set-all m fill))
;;     (%make-matrix m)))
