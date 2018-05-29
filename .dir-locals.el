;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode
  (eval . (setq-local
           compile-command
           (let
               ((file
                 (file-name-nondirectory buffer-file-name)))
             (format "csc -s %s -C -lgsl `pkg-config --cflags --libs gsl` -emit-all-import-libraries"
                     file))))))

