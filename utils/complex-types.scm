(import (only srfi-4 f64vector f32vector))
(define-foreign-type complex
  f64vector
  (lambda (z)
    (f64vector (real-part z) (imag-part z))))

(define-foreign-type complex-float
  f32vector
  (lambda (z)
    (f32vector (real-part z) (imag-part z))))

(cond-expand
  (complex
   (define-foreign-type TYPE
     f64vector
     (lambda (z)
       (f64vector (real-part z) (imag-part z)))))
  (complex-float
   (define-foreign-type TYPE
     f32vector
     (lambda (z)
       (f32vector (real-part z) (imag-part z)))))
  (else))
