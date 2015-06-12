;;; nix-env --- wrapper around the nix-env program

(defcustom nix-package-inferior-nix-env-program "nix-env"
  "Name of the program to use for nix-env commands."
  :type 'string
  :group 'nix-package)

(defun nix-package--nix-env (&rest args)
  "Run nix-env with the provided ARGS."
  (with-temp-buffer
    (let ((ret (apply #'call-process
                      nix-package-inferior-nix-env-program nil t nil
                      "--show-trace" "--xml" args)))
      (if (= ret 0)
          (libxml-parse-xml-region (point-min) (point-max))
        (error "nix failed with error code: %S\n%s" ret (buffer-string))))))

(defun nix-package-install (finish-func &rest args)
  "Run the install operation from nix-env. This can take forever, so it runs async."
  (apply #'async-start-process
         "nix-install" nix-package-inferior-nix-env-program finish-func "-i" args))

(defun nix-package-uninstall (&rest args)
  "Run the uninstall operation from nix-env."
  (apply #'nix-package--nix-env "--uninstall" args))

(defun nix-package-query-available (system-filter)
  "Query the Nix environment."
  (apply #'nix-package--nix-env
         "--query"
         "--available"
         "--status" "--compare-versions" "--system" "--description"
         (if system-filter `("--system-filter" ,system-filter))))

(defun nix-package-query-installed (system-filter)
  "Query the Nix environment."
  (apply #'nix-package--nix-env
         "--query"
         "--installed"
         "--status" "--compare-versions" "--system" "--description"
         (if system-filter `("--system-filter" ,system-filter))))

(provide 'nix-env)
