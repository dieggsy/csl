(module csl-complex ()
  (import chicken scheme foreign irregex extras)
  (use numbers)
  (foreign-declare "#include <gsl/gsl_complex.h>")
  (foreign-declare "#include <gsl/gsl_complex_math.h>")

  (define-syntax defcomplex
    (ir-macro-transformer
     (lambda (e i c)
       (define (make-gensym sym)
         (case (strip-syntax sym)
           ((double) (gensym "x"))
           ((complex) (gensym "z"))))
       (define (make-letvars args)
         (let* ((only-complex (filter (lambda (x)
                                        (eq? (car x) 'complex))
                                      args))
                (only-names (map cadr only-complex)))
           (fold (lambda (x y)
                   (cons
                    `(,(string->symbol (conc "r" x)) (real-part ,x))
                    (cons
                     `(,(string->symbol (conc "i" x)) (imag-part ,x))
                     y)))
                 '()
                 only-names)))
       (define (make-foreign-args* foreign-args)
         (fold (lambda (x y)
                 (if (eq? (car x) 'complex)
                     (cons `(double ,(string->symbol (conc "r" (cadr x))))
                           (cons `(double ,(string->symbol (conc "i" (cadr x))))
                                 y))
                     (cons x y)))
               '()
               foreign-args))
       (define (make-inits args)
         (let* ((only-complex (filter (lambda (x)
                                        (eq? (car x) 'complex))
                                      args)))
           (fold (lambda (x y)
                   (cons
                    (let ((name (cadr x)))
                      (format "gsl_complex ~a = gsl_complex_rect(~a,~a);"
                              (symbol->string name)
                              (conc "r" name)
                              (conc "i" name)))
                    y))
                 '()
                 only-complex)))
       (define (make-return ret-type fn names)
         (let ((strargs (string-join (map symbol->string names) ",")))
           (if (eq? ret-type 'scheme-object)
               `(,(format "gsl_complex out = gsl_complex_~a(~a);"
                          fn
                          strargs)
                 ,(format "C_return(numbers_make_rect(GSL_REAL(out),GSL_IMAG(out)));"))
               `(,(format "C_return(gsl_complex_~a(~a));"
                          fn
                          strargs)))))
       (let* ((name (cadadr e))
              (gsl-name (conc "gsl_complex_" name))
              (csl-name
               (string->symbol
                (conc "csl:complex-"
                      (if (null? (cddr e))
                          (string-downcase
                           (irregex-replace/all "_" name "-"))
                          (strip-syntax
                           (caddr e))))))
              (arg-types (map strip-syntax (cddadr e)))
              (arg-names (map make-gensym (cddadr e)))
              (foreign-args (zip arg-types arg-names))
              (foreign-args* (make-foreign-args* foreign-args))
              (ret-type (if (eq? (strip-syntax (caadr e)) 'complex)
                            'scheme-object
                            (strip-syntax (caadr e)))))
         `(begin
            (export ,(string->symbol gsl-name))
            (define (,(string->symbol gsl-name) ,@arg-names)
              (let (,@(make-letvars foreign-args))
                ((foreign-safe-lambda* ,ret-type ,foreign-args*
                   ,@(make-inits foreign-args)
                   ,@(make-return ret-type name arg-names))
                 ,@(map cadr foreign-args*))))
            (export ,csl-name)
            (define ,csl-name ,(string->symbol gsl-name)))))))

  ;; TODO concerns about memory allocation with complex numbers?
  (define-external (numbers_make_rect (double r) (double i)) scheme-object
    (make-rectangular r i))

  (defcomplex (complex "rect" double double))

  (defcomplex (complex "polar" double double))

  (defcomplex (double "arg" complex))

  (defcomplex (double "abs" complex))

  (defcomplex (double "abs2" complex))

  (defcomplex (double "logabs" complex))

  (defcomplex (complex "add" complex complex))
  (defcomplex (complex "sub" complex complex))
  (defcomplex (complex "mul" complex complex))
  (defcomplex (complex "div" complex complex))
  (defcomplex (complex "add_real" complex double))
  (defcomplex (complex "sub_real" complex double))
  (defcomplex (complex "mul_real" complex double))
  (defcomplex (complex "div_real" complex double))
  (defcomplex (complex "add_imag" complex double))
  (defcomplex (complex "sub_imag" complex double))
  (defcomplex (complex "mul_imag" complex double))
  (defcomplex (complex "div_imag" complex double))
  (defcomplex (complex "conjugate" complex))
  (defcomplex (complex "inverse" complex))
  (defcomplex (complex "negative" complex))

  (defcomplex (complex "sqrt" complex))
  (defcomplex (complex "sqrt_real" double))
  (defcomplex (complex "pow" complex complex) expt)
  (defcomplex (complex "pow_real" complex double) expt-real)
  (defcomplex (complex "exp" complex))
  (defcomplex (complex "log10" complex))

  (export gsl_complex_log)
  (define (gsl_complex_log z3488)
    (let ((rz3488 (real-part z3488)) (iz3488 (imag-part z3488)))
      ((foreign-safe-lambda* scheme-object ((double rz3488) (double iz3488))
         "gsl_complex z3488 = gsl_complex_rect(rz3488,iz3488);"
         "gsl_complex out = gsl_complex_log(z3488);"
         "C_return(numbers_make_rect(GSL_REAL(out),GSL_IMAG(out)));")
       rz3488
       iz3488)))
  (export gsl_complex_log_b)
  (define (gsl_complex_log_b z748 z749)
    (let ((rz749 (real-part z749))
          (iz749 (imag-part z749))
          (rz748 (real-part z748))
          (iz748 (imag-part z748)))
      ((foreign-safe-lambda*
           scheme-object
           ((double rz749) (double iz749) (double rz748) (double iz748))
         "gsl_complex z749 = gsl_complex_rect(rz749,iz749);"
         "gsl_complex z748 = gsl_complex_rect(rz748,iz748);"
         "gsl_complex out = gsl_complex_log_b(z748,z749);"
         "C_return(numbers_make_rect(GSL_REAL(out),GSL_IMAG(out)));")
       rz749
       iz749
       rz748
       iz748)))
  (export csl:complex-log)
  (define (csl:complex-log a #!optional b)
    (if b
        (gsl_complex_log_b a b)
        (gsl_complex_log a)))

  (defcomplex (complex "sin" complex))
  (defcomplex (complex "cos" complex))
  (defcomplex (complex "tan" complex))
  (defcomplex (complex "sec" complex))
  (defcomplex (complex "csc" complex))
  (defcomplex (complex "cot" complex))

  (defcomplex (complex "arcsin" complex) asin)
  (defcomplex (complex "arcsin_real" double) asin-real)

  (defcomplex (complex "arccos" complex) acos)
  (defcomplex (complex "arccos_real" double) acos-real)

  (defcomplex (complex "arctan" complex))

  (defcomplex (complex "arcsec" complex) asec)
  (defcomplex (complex "arcsec_real" double) asec-real)

  (defcomplex (complex "arccsc" complex) acsc)
  (defcomplex (complex "arccsc_real" double) acsc-real)

  (defcomplex (complex "arccot" complex))

  (defcomplex (complex "sinh" complex))
  (defcomplex (complex "cosh" complex))
  (defcomplex (complex "tanh" complex))
  (defcomplex (complex "sech" complex))
  (defcomplex (complex "csch" complex))
  (defcomplex (complex "coth" complex))

  (defcomplex (complex "arcsinh" complex) asinh)

  (defcomplex (complex "arccosh" complex) acosh)
  (defcomplex (complex "arccosh_real" double) acosh-real)

  (defcomplex (complex "arctanh" complex) atanh)
  (defcomplex (complex "arctanh_real" double) atanh-real)

  (defcomplex (complex "arcsech" complex) asech)
  (defcomplex (complex "arccsch" complex) acsch)
  (defcomplex (complex "arccoth" complex) acoth))
