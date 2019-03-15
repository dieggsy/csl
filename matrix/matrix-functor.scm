(functor (generic-matrix (M (matrix?
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
                             matrix-fread
                             matrix-fprintf
                             matrix-fscanf
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
    (list->matrix
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
     matrix-appendv
     matrix-column-ref
     matrix-column-set!
     matrix-fliplr
     matrix-fliplr!
     matrix-appendh
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
            (only chicken.module reexport)
            (only chicken.base add1 sub1 when cut foldl foldr identity)
            (only miscmacros ensure)
            (only srfi-1 every)
            (prefix M gsl:))

  (reexport M)
  (define (list->matrix lst)
    ;; (ensure (lambda (l)
    ;;           (and ((list-of? list?) l)
    ;;                (let ((len (length (car l))))
    ;;                  (every (lambda (x)
    ;;                           (= (length len)
    ;;                              (length x)))  (cdr l)))))
    ;;         "not a list of lists of the same length" l)
    (let* ((rows (length lst))
           (cols (length (car lst)))
           (m (gsl:matrix-alloc rows cols)))
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
    (let ((m (gsl:matrix-alloc rows cols)))
      (when fill
        (gsl:matrix-set-all! m fill))
      m))

  (define matrix-rows matrix-size1)

  (define matrix-columns matrix-size2)

  (define (matrix-map fn . matrices)
    (let* ((rows (apply min (map gsl:matrix-size1 matrices)))
           (cols (apply min (map gsl:matrix-size2 matrices)))
           (new (gsl:matrix-alloc rows cols)))
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
        (gsl:matrix-submatrix m row-start col-start (- row-end row-start) (- col-end col-start))
        (gsl:matrix-submatrix-with-stride m
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
    (let* ((new (gsl:matrix-alloc (gsl:matrix-size1 m) (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      new))

  (define matrix-copy! matrix-memcpy!)

  (define (matrix-swap-elements! m i0 j0 i1 j1)
    (let* ((tmp (gsl:matrix-get m i1 j1)))
      (gsl:matrix-set! m i1 j1 (gsl:matrix-get m i0 j0))
      (gsl:matrix-set! m i0 j0 tmp)))

  (define matrix-row-ref gsl:matrix-row)

  (define matrix-row-set! gsl:matrix-set-row!)

  (define (matrix-flipud! m)
    (do ((gr (sub1 (gsl:matrix-size1 m)) (sub1 gr))
         (sr 0 (add1 sr)))
        ((= gr -1))
      (let ((tmp (gsl:matrix-row m sr)))
        (gsl:matrix-set-row! m sr (gsl:matrix-row m gr))
        (gsl:matrix-set-row! m gr tmp))))

  (define (matrix-flipud m)
    (let ((new (gsl:matrix-alloc (gsl:matrix-size1 m)
                                 (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (matrix-flipud! new)
      new))

  (define (matrix-appendh . matrices)
    (define (row-append ptr1 ptr2)
      (let* ((len1 (gsl:matrix-size1 ptr1))
             (len2 (gsl:matrix-size1 ptr2))
             (newlen (+ len1 len2))
             (new (gsl:matrix-alloc newlen
                                    (gsl:matrix-size2 ptr1))))
        (do ((i 0 (add1 i)))
            ((= i len1))
          (gsl:matrix-set-row! new i (gsl:matrix-row ptr1 i)))
        (do ((i len1 (add1 i)))
            ((= i newlen))
          (gsl:matrix-set-row! new i (gsl:matrix-row ptr2 (- i len1))))
        new))
    (foldl row-append (car matrices) (cdr matrices)))

  (define matrix-column-ref matrix-column)

  (define matrix-column-set! gsl:matrix-set-col!)

  (define (matrix-fliplr! m)
    (do ((gr (sub1 (gsl:matrix-size2 m)) (sub1 gr))
         (sr 0 (add1 sr)))
        ((= gr -1))
      (let ((tmp (gsl:matrix-column m sr)))
        (gsl:matrix-set-col! m sr (gsl:matrix-column m gr))
        (gsl:matrix-set-col! m gr tmp))))

  (define (matrix-fliplr m)
    (let ((new (gsl:matrix-alloc (gsl:matrix-size1 m)
                                 (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (matrix-fliplr! new)
      new))

  (define (matrix-appendv . matrices)
    (define (column-append ptr1 ptr2)
      (let* ((len1 (gsl:matrix-size2 ptr1))
             (len2 (gsl:matrix-size2 ptr2))
             (newlen (+ len1 len2))
             (new (gsl:matrix-alloc (gsl:matrix-size1 ptr1)
                                    newlen)))
        (do ((i 0 (+ i 1)))
            ((= i len1))
          (gsl:matrix-set-col! new i (gsl:matrix-column ptr1 i)))
        (do ((i len1 (+ i 1)))
            ((= i newlen))
          (gsl:matrix-set-col! new i (gsl:matrix-column ptr2 (- i len1))))
        new))
    (foldl column-append (car matrices) (cdr matrices)))

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
           (new (gsl:matrix-alloc (gsl:matrix-size1 m1)
                                  (gsl:matrix-size2 m1))))
      (gsl:matrix-memcpy! new m1)
      (for-each (cut gsl:matrix-add! new <>) (cdr matrices))
      new))

  (define (matrix- . matrices)
    (let* ((single? (null? (cdr matrices)))
           (m1 (car matrices))
           (new (gsl:matrix-calloc (gsl:matrix-size1 m1)
                                   (gsl:matrix-size2 m1))))
      (when (not single?)
        (gsl:matrix-memcpy! new m1))
      (for-each (cut gsl:matrix-sub! new <>) ((if single? identity cdr) matrices))
      new))

  (define (matrix~* . matrices)
    (let* ((m1 (car matrices))
           (new (gsl:matrix-alloc (gsl:matrix-size1 m1)
                                  (gsl:matrix-size2 m1))))
      (gsl:matrix-memcpy! new m1)
      (for-each (cut gsl:matrix-mul-elements! new <>) (map matrix->ptr (cdr matrices)))
      new))

  (define (matrix~/ . matrices)
    (let* ((single? (null? (cdr matrices)))
           (m1 (car matrices))
           (new (gsl:matrix-alloc (gsl:matrix-size1 m1)
                                  (gsl:matrix-size2 m1))))
      (if single?
          (matrix-fill! new 1)
          (gsl:matrix-memcpy! new m1))
      (for-each (cut gsl:matrix-div-elements! new <>) ((if single? identity cdr) matrices))
      new))

  (define (matrix-scale m n)
    (let* ((new (gsl:matrix-alloc (gsl:matrix-size1 m)
                                  (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (gsl:matrix-scale! new n)
      new))

  (define (matrix-add-constant m n)
    (let* ((new (gsl:matrix-alloc (gsl:matrix-size1 m)
                                  (gsl:matrix-size2 m))))
      (gsl:matrix-memcpy! new m)
      (gsl:matrix-add-constant! new n)
      new))

  (define matrix-zero? gsl:matrix-isnull?)

  (define matrix-positive? gsl:matrix-ispos?)

  (define matrix-negative? gsl:matrix-isneg?)

  (define matrix-nonnegative? gsl:matrix-isnonneg?)

  (define (matrix= . matrices)
    (foldr (lambda (x y)
             (and
              (gsl:matrix-equal? (car matrices) x)
              y))
           #t
           (cdr matrices)))

  (define (make-identity-matrix n)
    (let ((ptr (gsl:matrix-alloc n n)))
      (gsl:matrix-set-identity! ptr)
      ptr))

  (define (matrix-diagonal* m #!optional (k 0))
    (if (negative? k)
        (gsl:matrix-subdiagonal m (- k))
        (gsl:matrix-superdiagonal m k)))

  (define (matrix-transpose m)
    (let* ((rows (gsl:matrix-size1 m))
           (cols (gsl:matrix-size2 m))
           (new (gsl:matrix-alloc cols rows)))
      (gsl:matrix-transpose-memcpy! new m)
      new)))


