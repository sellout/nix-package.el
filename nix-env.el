;;; nix-env --- wrapper around the nix-env program

(require 'json)
(require 'tabulated-list)

(defcustom nix-package-inferior-nix-env-program "nix-env"
  "Name of the program to use for nix-env commands."
  :type 'string
  :group 'nix-package)

(defun nix-package--nix-env (&rest args)
  "Run nix-env with the provided ARGS."
  (shell-command-to-string (intercalate (cons nix-package-inferior-nix-env-program args) " ")))

(defun nix-package-install (finish-func &rest args)
  "Run the install operation from nix-env. This can take forever, so it runs async."
  (apply #'async-start-process
         "nix-install" nix-package-inferior-nix-env-program finish-func "-i" args))

(defun nix-package-uninstall (&rest args)
  "Run the uninstall operation from nix-env."
  (apply #'nix-package--nix-env "--uninstall" args))

(defun nix-package-query-available ()
  "Query the Nix environment."
  (json-read-from-string (nix-package--nix-env "--query" "--available" "--json")))

(defun nix-package-query-installed ()
  "Query the Nix environment."
  (json-read-from-string (nix-package--nix-env "-q" "--json")))

(provide 'nix-env)
