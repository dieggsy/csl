;; (define fdopen (foreign-lambda (c-pointer "FILE") fdopen int c-string))
;; (define fclose (foreign-lambda int fclose (c-pointer "FILE")))
;; (define stdout (foreign-value stdout (c-pointer "FILE")))
;; (define stdin (foreign-value stdin (c-pointer "FILE")))

(import (only chicken.base port? assert))
(define (get-c-file fn-name port)
  (define extract-c-file
    (foreign-lambda* c-pointer ((scheme-object port))
                     "C_return(C_block_item(port, 0));"))
  (assert (port? port) fn-name "bad argument type - not a port" port)
  (assert (eq? 'stream (##sys#slot port 7)) fn-name "bad argument type - not a file port" port)
  (extract-c-file port))
