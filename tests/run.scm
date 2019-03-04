(import test)

(test-group "Polynomials"
  (import (csl poly))
  (test-group "GSL wrappers"
    (test "poly-eval" 86.0 (poly-eval '(1 2 3) 5)))
  (test-group "Pure scheme"
    (test "poly-eval*" 86 (poly-eval* '(1 2 3) 5))))





(test-exit)
