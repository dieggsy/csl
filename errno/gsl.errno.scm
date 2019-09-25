(module gsl.errno (set-error-handler!
                   strerror
                   errno->error
                   error-handler
                   errno-name-alist
                   ;; Error codes
                   errno/success
                   errno/failure
                   errno/continue
                   errno/dom
                   errno/range
                   errno/fault
                   errno/inval
                   errno/failed
                   errno/factor
                   errno/sanity
                   errno/nomem
                   errno/badfunc
                   errno/runaway
                   errno/maxiter
                   errno/zerodiv
                   errno/badtol
                   errno/tol
                   errno/undrflw
                   errno/ovrflw
                   errno/loss
                   errno/round
                   errno/badlen
                   errno/notsqr
                   errno/sing
                   errno/diverge
                   errno/unsup
                   errno/unimpl
                   errno/cache
                   errno/table
                   errno/noprog
                   errno/noprogj
                   errno/tolf
                   errno/tolx
                   errno/tolg
                   errno/eof)
  (import scheme
          chicken.foreign
          (only chicken.base void alist-ref make-parameter assert)
          (only chicken.condition condition signal))

  (foreign-declare "#include <gsl/gsl_errno.h>")

  (define error-handler
    (make-parameter
     'handler
     (lambda (v)
       (assert
        (or (eqv? v 'handler)
            (eqv? v 'return)
            (not v))
        'error-handler
        "value must be one of 'handler, 'return, or #f"
        v)
       v)))

  (define set-error-handler!
    (foreign-lambda void "gsl_set_error_handler" (c-pointer void)))

  (define strerror (foreign-lambda c-string "gsl_strerror" (const int)))

  (define (errno->error errno handler
                        #!optional (file 'unknown) (line 'unknown)
                        (reason (strerror errno)))
    (if (= errno errno/success)
        (if (error-handler) (void) errno)
        (if (eqv? (error-handler) handler)
            (signal (condition `(gsl file ,file line ,line handler ,handler)
                               `(,(alist-ref errno errno-name-alist = 'unknown-error))
                               `(exn message ,reason)))
            errno)))

  (define errno/success (foreign-value GSL_SUCCESS int))
  (define errno/failure (foreign-value GSL_FAILURE int))
  (define errno/continue (foreign-value GSL_CONTINUE int))
  (define errno/dom (foreign-value GSL_EDOM int))
  (define errno/range (foreign-value GSL_ERANGE int))
  (define errno/fault (foreign-value GSL_EFAULT int))
  (define errno/inval (foreign-value GSL_EINVAL int))
  (define errno/failed (foreign-value GSL_EFAILED int))
  (define errno/factor (foreign-value GSL_EFACTOR int))
  (define errno/sanity (foreign-value GSL_ESANITY int))
  (define errno/nomem (foreign-value GSL_ENOMEM int))
  (define errno/badfunc (foreign-value GSL_EBADFUNC int))
  (define errno/runaway (foreign-value GSL_ERUNAWAY int))
  (define errno/maxiter (foreign-value GSL_EMAXITER int))
  (define errno/zerodiv (foreign-value GSL_EZERODIV int))
  (define errno/badtol (foreign-value GSL_EBADTOL int))
  (define errno/tol (foreign-value GSL_ETOL int))
  (define errno/undrflw (foreign-value GSL_EUNDRFLW int))
  (define errno/ovrflw (foreign-value GSL_EOVRFLW int))
  (define errno/loss (foreign-value GSL_ELOSS int))
  (define errno/round (foreign-value GSL_EROUND int))
  (define errno/badlen (foreign-value GSL_EBADLEN int))
  (define errno/notsqr (foreign-value GSL_ENOTSQR int))
  (define errno/sing (foreign-value GSL_ESING int))
  (define errno/diverge (foreign-value GSL_EDIVERGE int))
  (define errno/unsup (foreign-value GSL_EUNSUP int))
  (define errno/unimpl (foreign-value GSL_EUNIMPL int))
  (define errno/cache (foreign-value GSL_ECACHE int))
  (define errno/table (foreign-value GSL_ETABLE int))
  (define errno/noprog (foreign-value GSL_ENOPROG int))
  (define errno/noprogj (foreign-value GSL_ENOPROGJ int))
  (define errno/tolf (foreign-value GSL_ETOLF int))
  (define errno/tolx (foreign-value GSL_ETOLX int))
  (define errno/tolg (foreign-value GSL_ETOLG int))
  (define errno/eof (foreign-value GSL_EOF int))

  (define errno-name-alist
    `((,errno/success . success)
      (,errno/failure . failure)
      (,errno/continue . continue)
      (,errno/dom . dom)
      (,errno/range . range)
      (,errno/fault . fault)
      (,errno/inval . inval)
      (,errno/failed . failed)
      (,errno/factor . factor)
      (,errno/sanity . sanity)
      (,errno/nomem . nomem)
      (,errno/badfunc . badfunc)
      (,errno/runaway . runaway)
      (,errno/maxiter . maxiter)
      (,errno/zerodiv . zerodiv)
      (,errno/badtol . badtol)
      (,errno/tol . tol)
      (,errno/undrflw . undrflw)
      (,errno/ovrflw . ovrflw)
      (,errno/loss . loss)
      (,errno/round . round)
      (,errno/badlen . badlen)
      (,errno/notsqr . notsqr)
      (,errno/sing . sing)
      (,errno/diverge . diverge)
      (,errno/unsup . unsup)
      (,errno/unimpl . unimpl)
      (,errno/cache . cache)
      (,errno/table . table)
      (,errno/noprog . noprog)
      (,errno/noprogj . noprogj)
      (,errno/tolf . tolf)
      (,errno/tolx . tolx)
      (,errno/tolg . tolg)
      (,errno/eof . eof))))
