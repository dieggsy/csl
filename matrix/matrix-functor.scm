(import-for-syntax (chicken string)
                   (except (srfi 13)
                           string->list
                           string-copy
                           string-fill!))
(define-syntax csl-matrix-module
  (er-macro-transformer
   (lambda (e i c)
     (let* ((matrix-module (cadr e))
            (vector-module (string->symbol
                            (apply string-append
                                   `("csl.vector."
                                     ,@(intersperse
                                        (cddr
                                         (string-split (symbol->string matrix-module) "."))
                                        "."))))))
       `(begin
          (functor (generic-matrix (M (matrix-alloc
                                       matrix-set!
                                       matrix-rows
                                       matrix-cols
                                       matrix-get
                                       matrix-set-all!
                                       matrix-set-identity!
                                       matrix-memcpy!
                                       matrix-get-row
                                       matrix-set-row!
                                       matrix-swap-rows!
                                       matrix-get-col
                                       matrix-set-col!
                                       matrix-swap-columns!
                                       matrix-transpose-memcpy!
                                       matrix-transpose!
                                       matrix-swap-rowcol!
                                       matrix-submatrix-with-stride
                                       matrix-add!
                                       matrix-sub!
                                       matrix-mul-elements!
                                       matrix-div-elements!
                                       matrix-scale!
                                       matrix-add-constant!
                                       matrix-isnull?
                                       matrix-ispositive?
                                       matrix-isnegative?
                                       matrix-isnonneg?
                                       matrix-equal?
                                       matrix-max
                                       matrix-min
                                       matrix-max-index
                                       matrix-min-index
                                       matrix-diagonal
                                       matrix-subdiagonal
                                       matrix-superdiagonal)))
              (matrix?
               matrix->ptr
               ptr->matrix
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
               matrix-argmax
               matrix-argmin
               make-identity-matrix
               matrix-identity!
               submatrix
               matrix-row-swap!
               matrix-column-swap!
               matrix-row-column-swap!
               matrix-diagonal
               matrix-subdiagonal
               matrix-superdiagonal
               matrix-transpose!
               matrix-transpose
               )
              (import (except scheme
                              vector?
                              list->vector
                              vector->list
                              vector
                              make-vector
                              vector-length
                              vector-ref
                              vector-set!
                              vector-fill!)
                      (except (chicken base) subvector vector-copy!)
                      (chicken foreign)
                      foreigners
                      (prefix M gsl:)
                      ,vector-module)

            (define-record-type matrix
              (ptr->matrix data)
              matrix?
              (data matrix->ptr))

            (define (list->matrix lst)
              (let* ((rows (length lst))
                     (cols (length (car lst)))
                     (m (gsl:matrix-alloc rows cols)))
                (do ((i 0 (+ i 1))
                     (l1 lst (cdr l1)))
                    ((= i rows) (ptr->matrix m))
                  (do ((j 0 (+ j 1))
                       (l2 (car l1) (cdr l2)))
                      ((= j cols))
                    (gsl:matrix-set! m i j (car l2))))))

            (define (matrix->list m)
              (let* ((data (matrix->ptr m))
                     (rows (gsl:matrix-rows data))
                     (columns (gsl:matrix-cols data)))
                (define (row->list n)
                  (do ((j (- columns 1) (- j 1))
                       (res '() (cons (gsl:matrix-get data n j) res)))
                      ((= j -1) res)))
                (do ((i (- rows 1) (- i 1))
                     (res '() (cons (row->list i) res)))
                    ((= i -1) res))))

            ;; (define (matrix . args)
            ;;   (list->matrix args))

            (define (make-matrix rows cols #!optional fill)
              (let ((m (gsl:matrix-alloc rows cols)))
                (when fill
                  (gsl:matrix-set-all! m fill))
                (ptr->matrix m)))

            (define (matrix-rows m)
              (let ((ptr (matrix->ptr m)))
                (gsl:matrix-rows ptr)))

            (define (matrix-columns m)
              (let ((ptr (matrix->ptr m)))
                (gsl:matrix-cols ptr)))

            (define (matrix-map fn . matrices)
              (let* ((ptrs (map matrix->ptr matrices))
                     (rows (apply min (map gsl:matrix-rows ptrs)))
                     (cols (apply min (map gsl:matrix-cols ptrs)))
                     (new (gsl:matrix-alloc rows cols)))
                (do ((i 0 (+ i 1)))
                    ((= i rows) (ptr->matrix new))
                  (do ((j 0 (+ j 1)))
                      ((= j cols))
                    (gsl:matrix-set! new i j (apply fn (map (cut gsl:matrix-get <> i j) ptrs)))))))

            (define (matrix-map! fn . matrices)
              (let* ((ptrs (map matrix->ptr matrices))
                     (rows (apply min (map gsl:matrix-rows ptrs)))
                     (cols (apply min (map gsl:matrix-cols ptrs))))
                (do ((i 0 (+ i 1)))
                    ((= i rows))
                  (do ((j 0 (+ j 1)))
                      ((= j cols))
                    (gsl:matrix-set! (car ptrs) i j (apply fn (map (cut gsl:matrix-get <> i j) ptrs)))))))

            (define (matrix-ref m i j)
              (gsl:matrix-get (matrix->ptr m) i j))

            (define (matrix-set! m i j val)
              (gsl:matrix-set! (matrix->ptr m) i j val))

            (define (matrix-fill! m n)
              (gsl:matrix-set-all! (matrix->ptr m) n)
              (void))

            (define (make-identity-matrix n)
              (let ((ptr (gsl:matrix-alloc n n)))
                (gsl:matrix-set-identity! ptr)
                (ptr->matrix ptr)))

            (define (matrix-identity! m)
              (gsl:matrix-set-identity! (matrix->ptr m)))

            (define (matrix-copy m)
              (let* ((ptr (matrix->ptr m))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr) (gsl:matrix-cols ptr))))
                (gsl:matrix-memcpy! new ptr)
                (ptr->matrix new)))

            (define (matrix-copy! m1 m2)
              (gsl:matrix-memcpy! (matrix->ptr m1) (matrix->ptr m2)))

            (define (matrix-swap! m i0 j0 i1 j1)
              (let* ((ptr (matrix->ptr m))
                     (tmp (gsl:matrix-get ptr i1 j1)))
                (gsl:matrix-set! ptr i1 j1 (gsl:matrix-get m i0 j0))
                (gsl:matrix-set! ptr i0 j0 tmp)))

            (define (matrix-row-ref m n)
              (ptr->vector (gsl:matrix-get-row (matrix->ptr m) n)))

            (define (matrix-row-set! m n v)
              (gsl:matrix-set-row! (matrix->ptr m) n (vector->ptr v))
              (void))

            (define (matrix-flipud m)
              (let* ((ptr (matrix->ptr m))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr)
                                            (gsl:matrix-cols ptr))))
                (gsl:matrix-memcpy! new ptr)
                (do ((gr (- (gsl:matrix-rows ptr) 1) (- gr 1))
                     (sr 0 (+ sr 1)))
                    ((= gr -1) (ptr->matrix new))
                  (let ((tmp (gsl:matrix-get-row new sr)))
                    (gsl:matrix-set-row! new sr (gsl:matrix-get-row new gr))
                    (gsl:matrix-set-row! new gr tmp)))))

            (define (matrix-flipud! m)
              (let ((ptr (matrix->ptr m)))
                (do ((gr (- (gsl:matrix-rows ptr) 1) (- gr 1))
                     (sr 0 (+ sr 1)))
                    ((= gr -1))
                  (let ((tmp (gsl:matrix-get-row ptr sr)))
                    (gsl:matrix-set-row! ptr sr (gsl:matrix-get-row ptr gr))
                    (gsl:matrix-set-row! ptr gr tmp)))
                (void)))

            (define (matrix-row-append . matrices)
              (define (row-append ptr1 ptr2)
                (let* ((len1 (gsl:matrix-rows ptr1))
                       (len2 (gsl:matrix-rows ptr2))
                       (newlen (+ len1 len2))
                       (new (gsl:matrix-alloc newlen
                                              (gsl:matrix-cols ptr1))))
                  (do ((i 0 (+ i 1)))
                      ((= i len1))
                    (gsl:matrix-set-row! new i (gsl:matrix-get-row ptr1 i)))
                  (do ((i len1 (+ i 1)))
                      ((= i newlen))
                    (gsl:matrix-set-row! new i (gsl:matrix-get-row ptr2 (- i len1))))
                  new))
              (let ((ptrs (map matrix->ptr matrices)))
                (ptr->matrix
                 (foldl row-append (car ptrs) (cdr ptrs)))))

            (define (matrix-row-swap! m i j)
              (gsl:matrix-swap-rows! (matrix->ptr m) i j))

            (define (matrix-column-ref m n)
              (ptr->vector (gsl:matrix-get-col (matrix->ptr m) n)))

            (define (matrix-column-set! m n v)
              (gsl:matrix-set-col! (matrix->ptr m) n (vector->ptr v))
              (void))

            (define (matrix-fliplr m)
              (let* ((ptr (matrix->ptr m))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr)
                                            (gsl:matrix-cols ptr))))
                (gsl:matrix-memcpy! new ptr)
                (do ((gr (- (gsl:matrix-cols ptr) 1) (- gr 1))
                     (sr 0 (+ sr 1)))
                    ((= gr -1) (ptr->matrix new))
                  (let ((tmp (gsl:matrix-get-col new sr)))
                    (gsl:matrix-set-col! new sr (gsl:matrix-get-col new gr))
                    (gsl:matrix-set-col! new gr tmp)))))

            (define (matrix-fliplr! m)
              (let ((ptr (matrix->ptr m)))
                (do ((gr (- (gsl:matrix-cols ptr) 1) (- gr 1))
                     (sr 0 (+ sr 1)))
                    ((= gr -1))
                  (let ((tmp (gsl:matrix-get-col ptr sr)))
                    (gsl:matrix-set-col! ptr sr (gsl:matrix-get-col ptr gr))
                    (gsl:matrix-set-col! ptr gr tmp)))
                (void)))

            (define (matrix-column-append . matrices)
              (define (column-append ptr1 ptr2)
                (let* ((len1 (gsl:matrix-cols ptr1))
                       (len2 (gsl:matrix-cols ptr2))
                       (newlen (+ len1 len2))
                       (new (gsl:matrix-alloc (gsl:matrix-rows ptr1)
                                              newlen)))
                  (do ((i 0 (+ i 1)))
                      ((= i len1))
                    (gsl:matrix-set-col! new i (gsl:matrix-get-col ptr1 i)))
                  (do ((i len1 (+ i 1)))
                      ((= i newlen))
                    (gsl:matrix-set-col! new i (gsl:matrix-get-col ptr2 (- i len1))))
                  new))
              (let ((ptrs (map matrix->ptr matrices)))
                (ptr->matrix
                 (foldl column-append (car ptrs) (cdr ptrs)))))

            (define (matrix-column-swap! m i j)
              (gsl:matrix-swap-columns! (matrix->ptr m) i j))

            (define (matrix-transpose m)
              (let* ((ptr (matrix->ptr m))
                     (rows (gsl:matrix-rows ptr))
                     (cols (gsl:matrix-cols ptr))
                     (new (gsl:matrix-alloc cols rows)))
                (gsl:matrix-transpose-memcpy! new ptr)
                (ptr->matrix new)))

            (define (matrix-transpose! m)
              (let ((ptr (matrix->ptr m)))
                (gsl:matrix-transpose! ptr)))

            (define (matrix-row-column-swap! m i j)
              (gsl:matrix-swap-rowcol! (matrix->ptr m) i j))

            (define (submatrix m i1 #!optional i2)
              (define (fix-index lst max)
                (let ((empty (null? lst)))
                  (list (or (and lst
                                 (not empty)
                                 (not (null? (cdr lst)))
                                 (car lst))
                            0)
                        (or (and lst
                                 (not empty)
                                 (not (null? (cdr lst)))
                                 (cadr lst))
                            (and lst
                                 (not (null? lst))
                                 (car lst))
                            max)
                        (or (and lst
                                 (not empty)
                                 (not (null? (cdr lst)))
                                 (not (null? (cddr lst)))
                                 (caddr lst))
                            1))))
              (let* ((ptr (matrix->ptr m))
                     (rows (gsl:matrix-rows ptr))
                     (cols (gsl:matrix-cols ptr))
                     (i1 (fix-index i1 rows))
                     (i2 (fix-index i2 cols))
                     (i1b (car i1))
                     (i1s (caddr i1))
                     (i1e (cadr i1))
                     (i2b (car i2))
                     (i2s (caddr i2))
                     (i2e (cadr i2)))
                (gsl:matrix-submatrix-with-stride ptr
                                                  i1b
                                                  i2b
                                                  i1s
                                                  i2s
                                                  (inexact->exact
                                                   (ceiling (/ (- i1e i1b) i1s)))
                                                  (inexact->exact
                                                   (ceiling (/ (- i2e i2b) i2s))))))

            (define (matrix+ . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices)))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr1)
                                            (gsl:matrix-cols ptr1))))
                (gsl:matrix-memcpy! new ptr1)
                (for-each (cut gsl:matrix-add! new <>) (map matrix->ptr (cdr matrices)))
                (ptr->matrix new)))

            (define (matrix+! . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices))))
                (for-each (cut gsl:matrix-add! ptr1 <>) (map matrix->ptr (cdr matrices)))))

            (define (matrix- . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices)))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr1)
                                            (gsl:matrix-cols ptr1))))
                (gsl:matrix-memcpy! new ptr1)
                (for-each (cut gsl:matrix-sub! new <>) (map matrix->ptr (cdr matrices)))
                (ptr->matrix new)))

            (define (matrix-! . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices))))
                (for-each (cut gsl:matrix-sub! ptr1 <>) (map matrix->ptr (cdr matrices)))))

            (define (matrix~* . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices)))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr1)
                                            (gsl:matrix-cols ptr1))))
                (gsl:matrix-memcpy! new ptr1)
                (for-each (cut gsl:matrix-mul-elements! new <>) (map matrix->ptr (cdr matrices)))
                (ptr->matrix new)))

            (define (matrix~*! . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices))))
                (for-each (cut gsl:matrix-mul-elements! ptr1 <>) (map matrix->ptr (cdr matrices)))))

            (define (matrix~/ . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices)))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr1)
                                            (gsl:matrix-cols ptr1))))
                (gsl:matrix-memcpy! new ptr1)
                (for-each (cut gsl:matrix-div-elements! new <>) (map matrix->ptr (cdr matrices)))
                (ptr->matrix new)))

            (define (matrix~/! . matrices)
              (let* ((ptr1 (matrix->ptr (car matrices))))
                (for-each (cut gsl:matrix-div-elements! ptr1 <>) (map matrix->ptr (cdr matrices)))))

            (define (matrix-scale v n)
              (let* ((ptr (matrix->ptr v))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr)
                                            (gsl:matrix-cols ptr))))
                (gsl:matrix-memcpy! new ptr)
                (gsl:matrix-scale! new n)
                (ptr->matrix new)))

            (define (matrix-scale! v n)
              (gsl:matrix-scale! (matrix->ptr v) n)
              (void))

            (define (matrix-add-constant v n)
              (let* ((ptr (matrix->ptr v))
                     (new (gsl:matrix-alloc (gsl:matrix-rows ptr)
                                            (gsl:matrix-cols ptr))))
                (gsl:matrix-memcpy! new ptr)
                (gsl:matrix-add-constant! new n)
                (ptr->matrix new)))

            (define (matrix-add-constant! v n)
              (gsl:matrix-add-constant! (matrix->ptr v) n)
              (void))

            (define (matrix-real-part m)
              (matrix-map real-part m))

            (define (matrix-imag-part m)
              (matrix-map imag-part m))

            (define (matrix-zero? v)
              (gsl:matrix-isnull? (matrix->ptr v)))

            (define (matrix-positive? v)
              (gsl:matrix-ispositive? (matrix->ptr v)))

            (define (matrix-negative? v)
              (gsl:matrix-isnegative? (matrix->ptr v)))

            (define (matrix-nonnegative? v)
              (gsl:matrix-isnonneg? (matrix->ptr v)))

            (define (matrix= . matrices)
              (foldr (lambda (x y)
                       (and
                        (gsl:matrix-equal? (matrix->ptr (car matrices)) x)
                        y))
                     #t
                     (map matrix->ptr (cdr matrices))))

            (define (matrix-max v)
              (gsl:matrix-max (matrix->ptr v)))

            (define (matrix-min v)
              (gsl:matrix-min (matrix->ptr v)))

            (define (matrix-argmax v)
              (gsl:matrix-max-index (matrix->ptr v)))

            (define (matrix-argmin v)
              (gsl:matrix-min-index (matrix->ptr v)))

            (define (matrix-diagonal m)
              (ptr->vector (gsl:matrix-diagonal (matrix->ptr m))))

            (define (matrix-subdiagonal m k)
              (ptr->vector (gsl:matrix-subdiagonal (matrix->ptr m) k)))

            (define (matrix-superdiagonal m k)
              (ptr->vector (gsl:matrix-superdiagonal (matrix->ptr m) k))))
          (module ,@(cdr e)))))))

