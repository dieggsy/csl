;; (define-record-type exact
;;   (make-exact num repeating)
;;   vector?
;;   (ptr vector->ptr))



(import (srfi 13)
        (chicken string))

(define (string-insert s i t) (string-replace s t i i))

;; (define repeating "1.0r42")

;; (import (chicken string))

(define (repeating->exact n)
  (let* ((dot (substring-index "." n))
         (r (substring-index "r" n))
         (p1 (- r dot 1))
         (p2 (+ p1 (- (string-length n) r 1)))
         (s1 (substring n 0 dot))
         (s2 (substring n (+ dot 1) r))
         (s3 (substring n (+ r 1) (string-length n)))
         (num1 (or (string->number (string-append s1 s2)) 0))
         (num2 (string->number (string-append s1 s2 s3))))
    (/ (- num2 num1)
       (- (expt 10 p2) (expt 10 p1)))))

(define (long-division n)
  (let loop ((num (numerator n))
             (den (denominator n))
             (seen '())
             (digits ""))
    (let ((rem (modulo num den))
          (quo (quotient num den)))
      (cond ((or (zero? rem)
                 (member rem seen))
             (let* ((seen (cons rem seen))
                    (digits (conc digits quo))
                    (len (length (member rem (reverse seen)))))
               (when (and (> rem 0) (> len 0))
                 (set! digits
                   (let ((max (string-length digits)))
                     (do ((i  max (- i 1))
                          (digits digits (string-insert digits i "\u0305"
                                                        )))
                         ((< i (- max (- len 2))) digits)))))
               (when (not (null? (cdr seen)))
                 (set! digits
                   (string-insert digits
                                  (string-length (number->string (modulo (numerator n)
                                                                         (denominator n))))
                                  ".")))

               digits))
            ((zero? quo)
             (loop (* 10 num) den (cons rem seen) (conc digits quo)))
            (else
             (loop (* 10 (- num (* quo den))) den (cons rem seen) (conc digits quo)))))))

;; (define exact->floatish )

;; (define exact-mode? (make-parameter #f))

;; (exact-mode? #t)

;; (define (##sys#string->number str #!optional (radix 10) exactness)
;;   (if (exact-mode?)
;;       (if (substring-index "r" str)
;;           (repeating->exact str)
;;           (string->number (string-append "#e" str)))
;;       (string->number str)))

;; (define ##sys#number->string )
;; (display(+ #e.000000000000000000001 #e0.010))
