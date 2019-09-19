(declare (unit csl.matrix))
(functor (generic-matrix (M (vector-free!
                             matrix?
                             matrix->ptr
                             ptr->matrix
                             matrix-size1
                             matrix-size2
                             matrix-alloc
                             matrix-calloc
                             matrix-free!
                             matrix-get
                             matrix-set!
                             matrix-set-all!
                             matrix-set-zero!
                             matrix-set-identity!
                             matrix-fwrite
                             matrix-fread!
                             matrix-fprintf
                             matrix-fscanf!
                             matrix-submatrix
                             matrix-submatrix-set!
                             matrix-submatrix-with-stride
                             matrix-submatrix-with-stride-set!
                             matrix-view-vector
                             matrix-row
                             matrix-column
                             matrix-subrow
                             matrix-subcolumn
                             matrix-diagonal
                             matrix-subdiagonal
                             matrix-superdiagonal
                             matrix-memcpy!
                             matrix-swap!
                             matrix-get-row!
                             matrix-get-col!
                             matrix-set-row!
                             matrix-set-col!
                             matrix-swap-rows!
                             matrix-swap-columns!
                             matrix-swap-rowcol!
                             matrix-transpose-memcpy!
                             matrix-transpose!
                             matrix-add!
                             matrix-sub!
                             matrix-mul-elements!
                             matrix-div-elements!
                             matrix-scale!
                             matrix-add-constant!
                             matrix-max
                             matrix-min
                             matrix-minmax
                             matrix-max-index
                             matrix-min-index
                             matrix-minmax-index
                             matrix-isnull?
                             matrix-ispos?
                             matrix-isneg?
                             matrix-isnonneg?
                             matrix-equal?)))
    (matrix-alloc
     matrix-calloc
     matrix-submatrix
     matrix-submatrix-with-stride
     matrix-view-vector
     matrix-row
     matrix-column
     matrix-subrow
     matrix-subcolumn
     matrix-diagonal
     matrix-subdiagonal
     matrix-superdiagonal
     list->matrix
     matrix->list
     matrix
     make-matrix
     matrix-rows
     matrix-columns
     matrix-map
     matrix-map!
     matrix-ref
     submatrix
     submatrix*
     submatrix-set!
     submatrix-set!*
     submatrix-call!
     submatrix-call!*
     matrix-fill!
     matrix-copy
     matrix-copy!
     matrix-swap-elements!
     matrix-row-ref
     matrix-row-set!
     matrix-flipud
     matrix-flipud!
     matrix-append-horizontal
     matrix-column-ref
     matrix-column-set!
     matrix-fliplr
     matrix-fliplr!
     matrix-append-vertical
     matrix-real-part
     matrix-imag-part
     matrix+
     matrix-
     matrix~*
     matrix~/
     matrix-scale
     matrix-add-constant
     matrix-zero?
     matrix-positive?
     matrix-negative?
     matrix-nonnegative?
     matrix=
     make-identity-matrix
     matrix-diagonal*
     matrix-transpose)
    (import scheme
            (only chicken.gc set-finalizer!)
            (only chicken.module reexport)
            (only chicken.base add1 sub1 when cut foldl foldr identity)
            (prefix M gsl:))

  (reexport (except M
                    matrix-alloc
                    matrix-calloc
                    matrix-submatrix
                    matrix-submatrix-with-stride
                    matrix-view-vector
                    matrix-row
                    matrix-column
                    matrix-subrow
                    matrix-subcolumn
                    matrix-diagonal
                    matrix-subdiagonal
                    matrix-superdiagonal))
  ;; Finalized versions of gsl.matrix
  (define (matrix-alloc i j)
    (set-finalizer! (gsl:matrix-alloc i j) matrix-free!))
  (define (matrix-calloc i j)
    (set-finalizer! (gsl:matrix-calloc i j) matrix-free!))
  (define (matrix-submatrix m k1 k2 n1 n2)
    (set-finalizer! (gsl:matrix-submatrix m k1 k2 n1 n2) matrix-free!))
  (define (matrix-submatrix-with-stride m k1 k2 s1 s2 m1 m2)
    (set-finalizer! (gsl:matrix-submatrix-with-stride m k1 k2 s1 s2 m1 m2) matrix-free!))
  (define (matrix-view-vector v n1 n2)
    (set-finalizer! (gsl:matrix-view-vector v n1 n2) matrix-free!))
  (define (matrix-row m i)
    (set-finalizer! (gsl:matrix-row m i) vector-free!))
  (define (matrix-column m j)
    (set-finalizer! (gsl:matrix-column m j) vector-free!))
  (define (matrix-subrow m i offset n)
    (set-finalizer! (gsl:matrix-subrow m i offset n) vector-free!))
  (define (matrix-subcolumn m j offset n)
    (set-finalizer! (gsl:matrix-subcolumn m j offset n) vector-free!))
  (define (matrix-diagonal m)
    (set-finalizer! (gsl:matrix-diagonal m) vector-free!))
  (define (matrix-subdiagonal m k)
    (set-finalizer! (gsl:matrix-subdiagonal m k) vector-free!))
  (define (matrix-superdiagonal m k)
    (set-finalizer! (gsl:matrix-superdiagonal m k) vector-free!))

  (define (list->matrix lst)
    (let* ((rows (length lst))
           (cols (length (car lst)))
           (m (matrix-alloc rows cols)))
      (do ((i 0 (add1 i))
           (l1 lst (cdr l1)))
          ((= i rows) m)
        (do ((j 0 (add1 j))
             (l2 (car l1) (cdr l2)))
            ((= j cols))
          (gsl:matrix-set! m i j (car l2))))))

  (define (matrix->list m)
    (let* ((rows (gsl:matrix-size1 m))
           (columns (gsl:matrix-size2 m)))
      (define (row->list n)
        (do ((j (sub1 columns) (sub1 j))
             (res '() (cons (gsl:matrix-get m n j) res)))
            ((= j -1) res)))
      (do ((i (sub1 rows) (sub1 i))
           (res '() (cons (row->list i) res)))
          ((= i -1) res))))

  (define-syntax matrix
    (syntax-rules ()
      ((_ arg)
       (list->matrix 'arg))))

  (define (make-matrix rows cols #!optional fill)
    (let ((m (matrix-calloc rows cols)))
      (when fill
        (gsl:matrix-set-all! m fill))
      m))

  (define matrix-rows matrix-size1)

  (define matrix-columns matrix-size2)

  (define (matrix-map fn . matrices)
    (let* ((rows (apply min (map gsl:matrix-size1 matrices)))
           (cols (apply min (map gsl:matrix-size2 matrices)))
           (new (matrix-alloc rows cols)))
      (do ((i 0 (+ i 1)))
          ((= i rows) new)
        (do ((j 0 (+ j 1)))
            ((= j cols))
          (gsl:matrix-set!
           new i j
           (apply fn (map (cut gsl:matrix-get <> i j) matrices)))))))

  (define (matrix-map! fn . matrices)
    (let* ((rows (apply min (map gsl:matrix-size1 matrices)))
           (cols (apply min (map gsl:matrix-size2 matrices))))
      (do ((i 0 (+ i 1)))
          ((= i rows))
        (do ((j 0 (+ j 1)))
            ((= j cols))
          (gsl:matrix-set!
           (car matrices) i j
           (apply fn (map (cut gsl:matrix-get <> i j) matrices)))))))

  (define matrix-ref matrix-get)

  (define (submatrix m
                     #!optional
                     (row-start 0)
                     (row-end (matrix-rows m))
                     (row-step 1)
                     (col-start 0)
                     (col-end (matrix-columns m))
                     (col-step 1))
    (if (= 1 row-step col-step)
        (matrix-submatrix m row-start col-start (- row-end row-start) (- col-end col-start))
        (matrix-submatrix-with-stride m
                                      row-start col-start
                                      row-step col-step
                                      (add1 (quotient (- row-end 1 row-start) row-step))
                                      (add1 (quotient (- col-end 1 col-start) col-step)))))

  (define (submatrix* m
                      #!key
                      (row-start 0)
                      (row-end (matrix-rows m))
                      (row-step 1)
                      (col-start 0)
                      (col-end (matrix-columns m))
                      (col-step 1))
    (submatrix m row-start row-end row-step col-start col-end col-step))

  (define (submatrix-set! m sub
                          #!optional
                          (row-start 0)
                          (row-end (matrix-rows m))
                          (row-step 1)
                          (col-start 0)
                          (col-end (matrix-columns m))
                          (col-step 1))
    (if (= 1 row-step col-step)
        (gsl:matrix-submatrix-set! m
                                   row-start col-start
                                   (- row-end row-start)
                                   (- col-end col-start)
                                   sub)
        (gsl:matrix-submatrix-with-stride-set! m
                                               row-start col-start
                                               row-step col-step
                                               (add1 (quotient (- row-end 1 row-start) row-step))
                                               (add1 (quotient (- col-end 1 col-start) col-step))
                                               sub)))

  (define (submatrix-set!* m sub
                           #!key
                           (row-start 0)
                           (row-end (matrix-rows m))
                           (row-step 1)
                           (col-start 0)
                           (col-end (matrix-columns m))
                           (col-step 1))
    (submatrix-set! m sub row-start row-end row-step col-start col-end col-step))

  (define (submatrix-call! f m
                           #!optional
                           (row-start 0)
                           (row-end (matrix-rows m))
                           (row-step 1)
                           (col-start 0)
                           (col-end (matrix-columns m))
                           (col-step 1))
    (let* ((sub (submatrix m row-start row-end row-step col-start col-end col-step))
           (rep (f sub)))
      (submatrix-set! m (if (matrix? rep) rep sub)
                      row-start row-end row-step col-start col-end col-step)))

  (define (submatrix-call!* f m
                            #!key
                            (row-start 0)
                            (row-end (matrix-rows m))
                            (row-step 1)
                            (col-start 0)
                            (col-end (matrix-columns m))
                            (col-step 1))
    (submatrix-call! f m row-start row-end row-step col-start col-end col-step))

  (define matrix-fill! matrix-set-all!)


  (define (matrix-copy m)
    (let* ((new (matrix-alloc (gsl:matrix-size1 m) (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      new))

  (define matrix-copy! matrix-memcpy!)

  (define (matrix-swap-elements! m i0 j0 i1 j1)
    (let* ((tmp (gsl:matrix-get m i1 j1)))
      (gsl:matrix-set! m i1 j1 (gsl:matrix-get m i0 j0))
      (gsl:matrix-set! m i0 j0 tmp)))

  (define matrix-row-ref matrix-row)

  (define matrix-row-set! gsl:matrix-set-row!)

  (define (matrix-flipud! m)
    (do ((gr (sub1 (gsl:matrix-size1 m)) (sub1 gr))
         (sr 0 (add1 sr)))
        ((= gr -1))
      (let ((tmp (matrix-row m sr)))
        (gsl:matrix-set-row! m sr (matrix-row m gr))
        (gsl:matrix-set-row! m gr tmp))))

  (define (matrix-flipud m)
    (let ((new (matrix-alloc (gsl:matrix-size1 m)
                                 (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (matrix-flipud! new)
      new))

  (define (matrix-append-vertical . matrices)
    (let* ((rows (cons 0 (map gsl:matrix-size1 matrices)))
           (newrows (apply + rows))
           (cols (gsl:matrix-size2 (car matrices)))
           (new (matrix-alloc newrows cols)))
      (do ((rs rows (cdr rs))
           (ms matrices (cdr ms)))
          ((null? ms) new)
        (matrix-submatrix-set! new (car rs) 0 (cadr rs) cols (car ms)))))

  (define matrix-column-ref matrix-column)

  (define matrix-column-set! gsl:matrix-set-col!)

  (define (matrix-fliplr! m)
    (do ((gr (sub1 (gsl:matrix-size2 m)) (sub1 gr))
         (sr 0 (add1 sr)))
        ((= gr -1))
      (let ((tmp (matrix-column m sr)))
        (gsl:matrix-set-col! m sr (matrix-column m gr))
        (gsl:matrix-set-col! m gr tmp))))

  (define (matrix-fliplr m)
    (let ((new (matrix-alloc (gsl:matrix-size1 m)
                                 (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (matrix-fliplr! new)
      new))


  (define (matrix-append-horizontal . matrices)
    (let* ((cols (cons 0 (map gsl:matrix-size2 matrices)))
           (newcols (apply + cols))
           (rows (gsl:matrix-size1 (car matrices)))
           (new (matrix-alloc rows newcols)))
      (do ((cs cols (cdr cs))
           (ms matrices (cdr ms)))
          ((null? ms) new)
        (matrix-submatrix-set! new 0 (car cs) rows (cadr cs) (car ms)))))

  (define (matrix-real-part m)
    (let* ((rows (matrix-size1 m))
           (cols (matrix-size2 m))
           (new (matrix-alloc rows cols)))
      (do ((i 0 (add1 i)))
          ((= i rows) new)
        (do ((j 0 (add1 j)))
            ((= j cols))
          (matrix-set! new i j (real-part (matrix-ref m i j)))))))

  (define (matrix-imag-part m)
    (let* ((rows (matrix-size1 m))
           (cols (matrix-size2 m))
           (new (matrix-alloc rows cols)))
      (do ((i 0 (add1 i)))
          ((= i rows) new)
        (do ((j 0 (add1 j)))
            ((= j cols))
          (matrix-set! new i j (imag-part (matrix-ref m i j)))))))

  (define (matrix+ . matrices)
    (let* ((m1 (car matrices))
           (new (matrix-calloc (gsl:matrix-size1 m1)
                               (gsl:matrix-size2 m1))))
      (gsl:matrix-memcpy! new m1)
      (for-each (cut gsl:matrix-add! new <>) (cdr matrices))
      new))

  (define (matrix- . matrices)
    (let* ((m1 (car matrices))
           (new (matrix-calloc (gsl:matrix-size1 m1)
                               (gsl:matrix-size2 m1))))
      (if (null? (cdr matrices))
          (gsl:matrix-sub! new (car matrices))
          (begin
            (gsl:matrix-memcpy! new m1)
            (for-each (cut gsl:matrix-sub! new <>) (cdr matrices))))
      new))

  (define (matrix~* . matrices)
    (let* ((m1 (car matrices))
           (new (matrix-alloc (gsl:matrix-size1 m1)
                                  (gsl:matrix-size2 m1))))
      (gsl:matrix-memcpy! new m1)
      (for-each (cut gsl:matrix-mul-elements! new <>) (map matrix->ptr (cdr matrices)))
      new))

  (define (matrix~/ . matrices)
    (let* ((m1 (car matrices))
           (new (matrix-alloc (gsl:matrix-size1 m1)
                              (gsl:matrix-size2 m1))))
      (if (null? (cdr matrices))
          (begin
            (matrix-fill! new 1)
            (gsl:matrix-div-elements! new (car matrices)))
          (begin
            (gsl:matrix-memcpy! new m1)
            (for-each (cut gsl:matrix-div-elements! new <>) (cdr matrices))))
      new))

  (define (matrix-scale m n)
    (let* ((new (matrix-alloc (gsl:matrix-size1 m)
                                  (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (gsl:matrix-scale! new n)
      new))

  (define (matrix-add-constant m n)
    (let* ((new (matrix-alloc (gsl:matrix-size1 m)
                                  (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (gsl:matrix-add-constant! new n)
      new))

  (define matrix-zero? gsl:matrix-isnull?)

  (define matrix-positive? gsl:matrix-ispos?)

  (define matrix-negative? gsl:matrix-isneg?)

  (define matrix-nonnegative? gsl:matrix-isnonneg?)

  ;; TODO: try short circuiting?
  (define (matrix= . matrices)
    (let ((first (car matrices)))
      (let loop ((e #t)
                 (ms (cdr matrices)))
        (if (null? ms)
            e
            (loop (gsl:matrix-equal? first (car ms)) (cdr ms))))))

  (define (make-identity-matrix n)
    (let ((ptr (matrix-alloc n n)))
      (gsl:matrix-set-identity! ptr)
      ptr))

  (define (matrix-diagonal* m #!optional (k 0))
    (if (negative? k)
        (matrix-subdiagonal m (- k))
        (matrix-superdiagonal m k)))

  (define (matrix-transpose m)
    (let* ((rows (gsl:matrix-size1 m))
           (cols (gsl:matrix-size2 m))
           (new (matrix-alloc cols rows)))
      (gsl:matrix-transpose-memcpy! new m)
      new)))

(import gsl.matrix.complex.double)
(import gsl.matrix.complex.float)
(import gsl.matrix.double)
(import gsl.matrix.float)
(import gsl.matrix.long)
(import gsl.matrix.int)
(import gsl.matrix.short)
(import gsl.matrix.char)
(import gsl.matrix.ulong)
(import gsl.matrix.uint)
(import gsl.matrix.ushort)
(import gsl.matrix.uchar)


(module csl.matrix.complex.double = (generic-matrix gsl.matrix.complex.double))
(module csl.matrix.complex.float = (generic-matrix gsl.matrix.complex.float))
(module csl.matrix.double = (generic-matrix gsl.matrix.double))
(module csl.matrix.float = (generic-matrix gsl.matrix.float))
(module csl.matrix.long = (generic-matrix gsl.matrix.long))
(module csl.matrix.int = (generic-matrix gsl.matrix.int))
(module csl.matrix.short = (generic-matrix gsl.matrix.short))
(module csl.matrix.char = (generic-matrix gsl.matrix.char))
(module csl.matrix.ulong = (generic-matrix gsl.matrix.ulong))
(module csl.matrix.uint = (generic-matrix gsl.matrix.uint))
(module csl.matrix.ushort = (generic-matrix gsl.matrix.ushort))
(module csl.matrix.uchar = (generic-matrix gsl.matrix.uchar))
