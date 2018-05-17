;; (compile-file "gsl.scm" options: '("-C" "-lgsl" "`pkg-config --cflags --libs gsl`"))
(module csl *
  (import scheme
          chicken
          extras
          data-structures
          ports
          foreign)
  (use (prefix numbers %)
       bind
       foreigners
       generics
       fmt)

  (include "gsl-math.scm")
  (include "gsl-vector.scm")
  (include "gsl-matrix.scm")
  (include "csl-vector.scm")
  (include "csl-matrix.scm")
  ;; (include "gsl-complex.scm")

  )
