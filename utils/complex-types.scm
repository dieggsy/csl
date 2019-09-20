(import srfi-4)
(define-foreign-type complex
  f64vector
  (lambda (z)
    (f64vector (real-part z) (imag-part z))))

(define-foreign-type complex-float
  f32vector
  (lambda (z)
    (f32vector (real-part z) (imag-part z))))
