;;; nix-package --- package.el-like for Nix

(require 'cl-lib)
(require 'nix-env)
(require 'tabulated-list)

;;;###autoload
(defun nix-package-describe-package (package)
  "Display the full documentation of PACKAGE (a symbol)."
  (interactive
   (let* ((guess (function-called-at-point)))
     (require 'finder-inf nil t)
     ;; Load the package list if necessary (but don't activate them).
     (unless package--initialized
       (package-initialize t))
     (let ((packages (append (mapcar 'car package-alist)
                             (mapcar 'car package-archive-contents)
                             (mapcar 'car package--builtins))))
       (unless (memq guess packages)
         (setq guess nil))
       (setq packages (mapcar 'symbol-name packages))
       (let ((val
              (completing-read (if guess
                                   (format "Describe package (default %s): "
                                           guess)
                                 "Describe package: ")
                               packages nil t nil nil guess)))
         (list (intern val))))))
  (if (not (or (package-desc-p package) (and package (symbolp package))))
      (message "No package specified")
    (help-setup-xref (list #'nix-package-describe-package package)
		     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
	(describe-package-1 package)))))

;;;###autoload
(defun list-nix-packages ()
  "Display a list of all known packages, akin to package.el."
  (interactive)
  (let (old-archives installed-pkgs)
    (setq installed-pkgs (nix-package-query-installed))
    (let ((buf (get-buffer-create "*Nix Packages*")))
      (with-current-buffer buf
	(nix-package-menu-mode)
	(nix-package-menu--generate nil t))
      (switch-to-buffer buf))))

(defalias 'nix-package-list-nix-packages 'list-nix-packages)

(defvar nix-package-menu-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "Nix Package")))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'nix-package-menu-describe-package)
    (define-key map "u" 'nix-package-menu-mark-unmark)
    (define-key map "\177" 'nix-package-menu-backup-unmark)
    (define-key map "d" 'nix-package-menu-mark-delete)
    (define-key map "i" 'nix-package-menu-mark-install)
    (define-key map "U" 'nix-package-menu-mark-upgrades)
    (define-key map "r" 'nix-package-menu-refresh)
    (define-key map "f" 'nix-package-menu-filter)
    (define-key map "~" 'nix-package-menu-mark-obsolete-for-deletion)
    (define-key map "x" 'nix-package-menu-execute)
    (define-key map "h" 'nix-package-menu-quick-help)
    (define-key map "?" 'nix-package-menu-describe-package)
    (define-key map [menu-bar nix-package-menu] (cons "Nix Package" menu-map))
    (define-key menu-map [mq]
      '(menu-item "Quit" quit-window
		  :help "Quit package selection"))
    (define-key menu-map [s1] '("--"))
    (define-key menu-map [mn]
      '(menu-item "Next" next-line
		  :help "Next Line"))
    (define-key menu-map [mp]
      '(menu-item "Previous" previous-line
		  :help "Previous Line"))
    (define-key menu-map [s2] '("--"))
    (define-key menu-map [mu]
      '(menu-item "Unmark" nix-package-menu-mark-unmark
		  :help "Clear any marks on a package and move to the next line"))
    (define-key menu-map [munm]
      '(menu-item "Unmark Backwards" nix-package-menu-backup-unmark
		  :help "Back up one line and clear any marks on that package"))
    (define-key menu-map [md]
      '(menu-item "Mark for Deletion" nix-package-menu-mark-delete
		  :help "Mark a package for deletion and move to the next line"))
    (define-key menu-map [mi]
      '(menu-item "Mark for Install" nix-package-menu-mark-install
		  :help "Mark a package for installation and move to the next line"))
    (define-key menu-map [mupgrades]
      '(menu-item "Mark Upgradable Packages" nix-package-menu-mark-upgrades
		  :help "Mark packages that have a newer version for upgrading"))
    (define-key menu-map [s3] '("--"))
    (define-key menu-map [mf]
      '(menu-item "Filter Package List..." nix-package-menu-filter
		  :help "Filter package selection (q to go back)"))
    (define-key menu-map [mg]
      '(menu-item "Update Package List" revert-buffer
		  :help "Update the list of packages"))
    (define-key menu-map [mr]
      '(menu-item "Refresh Package List" nix-package-menu-refresh
		  :help "Download the ELPA archive"))
    (define-key menu-map [s4] '("--"))
    (define-key menu-map [mt]
      '(menu-item "Mark Obsolete Packages" nix-package-menu-mark-obsolete-for-deletion
		  :help "Mark all obsolete packages for deletion"))
    (define-key menu-map [mx]
      '(menu-item "Execute Actions" nix-package-menu-execute
		  :help "Perform all the marked actions"))
    (define-key menu-map [s5] '("--"))
    (define-key menu-map [mh]
      '(menu-item "Help" nix-package-menu-quick-help
		  :help "Show short key binding help for nix-package-menu-mode"))
    (define-key menu-map [mc]
      '(menu-item "Describe Package" nix-package-menu-describe-package
		  :help "Display information about this package"))
    map)
  "Local keymap for `nix-package-menu-mode' buffers.")

(define-derived-mode nix-package-menu-mode tabulated-list-mode "Nix Package Menu"
  "Major mode for browsing a list of Nix packages.
Letters do not insert themselves; instead, they are commands.
\\<nix-package-menu-mode-map>
\\{nix-package-menu-mode-map}"
  (setq tabulated-list-format
        `[("Package" 18 nix-package-menu--name-predicate)
          ("Version" 10 nil)
          ("Channel"  7 nix-package-menu--channel-predicate)
          ("Group"   17 nil)
          ("Status"   6 nix-package-menu--status-predicate)
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Package" nil))
  (add-hook 'tabulated-list-revert-hook 'nix-package-menu--refresh nil t)
  (tabulated-list-init-header))

(defun nix-package-menu--name-predicate (A B)
  "Grrr A B."
  (string< (car (aref (cadr A) 0))
	   (car (aref (cadr B) 0))))

(defun nix-package-menu--channel-predicate (A B)
  "Grrr A B."
  (string< (symbol-name (car A))
	   (symbol-name (car B))))

(defun nix-package-menu--status-predicate (A B)
  "Grrr A B."
  (let* ((sA (aref (cadr A) 4))
         (sB (aref (cadr B) 4))
         (sA0 (elt sA 0))
         (sB0 (elt sB 0))
         (sA1 (elt sA 1))
         (sB1 (elt sB 1)))
    (cond ((= sA0 sB0)
           (cond ((= sA1 sB1) (nix-package-menu--name-predicate A B))
                 ((= sA1 ?\>) t)
                 ((= sB1 ?\>) nil)
                 ((= sA1 ?\ ) t)
                 ((= sB1 ?\ ) nil)))
          ((= sA0 ?I) t)
          ((= sB0 ?I) nil)
          ((= sA0 ?P) t)
          ((= sB0 ?P) nil)
          ((= sA0 ?S) t)
          ((= sB0 ?S) nil)
          ((= sA0 ?\ ) t)
          ((= sB0 ?\ ) nil))))

(defun nix-package-menu--refresh (&optional packages keywords)
  "Re-populate the `tabulated-list-entries'.
PACKAGES should be nil or t, which means to display all known packages.
KEYWORDS should be nil or a list of keywords."
  (cl-flet ((attr (field elem) (cdr (assoc field (cadr elem)))))
    (setq tabulated-list-entries
          (mapcar (lambda (pkg)
                    (nix-package-menu--print-info
                     `((id     . ,(attr 'attrPath pkg))
                       (name   . ,(attr 'name pkg))
                       (status . ,(concat (cond
                                           ((equal "1" (attr 'installed pkg))
                                            "I")
                                           ((equal "1" (attr 'valid pkg))
                                            "P")
                                           ((equal "1" (attr 'substitutable pkg))
                                            "S")
                                           ((equal "unknown" (attr 'system pkg))
                                            "D")
                                           (t " "))
                                          (case (attr 'versionDiff pkg)
                                            ("<" "<")
                                            (">" ">")
                                            (otherwise " "))))
                       (description . ,(attr 'description pkg)))))
                  (cddr (nix-package-query-available))))))

(defun nix-package-menu-get-status ()
  (let* ((id (tabulated-list-get-id))
	 (entry (and id (assq id tabulated-list-entries))))
    (if entry
	(aref (cadr entry) 4)
      "")))

(defun nix-package-menu-mark-delete (&optional _num)
  "Mark a package for deletion and move to the next line."
  (interactive "p")
  (if (member (nix-package-menu-get-status) '("insta" "obsol" "unsig"))
      (tabulated-list-put-tag "D" t)
    (forward-line)))

(defun nix-package-menu-mark-install (&optional _num)
  "Mark a package for installation and move to the next line."
  (interactive "p")
  (if (member (nix-package-menu-get-status) '("" "new" "disab"))
      (tabulated-list-put-tag "I" t)
    (forward-line)))

(defun nix-package-menu-mark-unmark (&optional _num)
  "Clear any marks on a package and move to the next line."
  (interactive "p")
  (tabulated-list-put-tag " " t))

(defun nix-package-menu-backup-unmark ()
  "Back up one line and clear any marks on that package."
  (interactive)
  (forward-line -1)
  (tabulated-list-put-tag " "))

;;;###autoload
(defun package-install (pkg)
  "Install the package PKG.
PKG can be a package-desc or the package name of one the available packages
in an archive in `package-archives'.  Interactively, prompt for its name."
  ;; (interactive
  ;;  (progn
  ;;    (list (intern (completing-read
  ;;                   "Install package: "
  ;;                   (delq nil
  ;;                         (mapcar (lambda (elt)
  ;;                                   (unless (package-installed-p (car elt))
  ;;                                     (symbol-name (car elt))))
  ;;                                 package-archive-contents))
  ;;                   nil t)))))
  (nix-package-install nil "-A" (symbol-name pkg))
  (package-download-transaction
   (if (package-desc-p pkg)
       (package-compute-transaction (list pkg)
                                    (package-desc-reqs pkg))
     (package-compute-transaction ()
                                  (list (list pkg))))))

(defun nix-package-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.
Optional argument NOQUERY non-nil means do not ask the user to confirm."
  (interactive)
  (unless (derived-mode-p 'nix-package-menu-mode)
    (error "The current buffer is not in Package Menu mode"))
  (let (install-list delete-list cmd pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq cmd (char-after))
	(unless (eq cmd ?\s)
	  ;; This is the key PKG-DESC.
	  (setq pkg-desc (symbol-name (tabulated-list-get-id)))
	  (cond ((eq cmd ?D)
		 (push pkg-desc delete-list))
		((eq cmd ?I)
		 (push pkg-desc install-list))))
	(forward-line)))
    (when install-list
      (if (or
           noquery
           (yes-or-no-p
            (if (= (length install-list) 1)
                (format "Install package `%s'? " (car install-list))
              (format "Install these %d packages (%s)? "
                      (length install-list)
                      (intercalate install-list ", ")))))
	  (apply #'nix-package-install
                 (lambda (x)
                   ,(async-inject-variables "\\`tablulated-list-")
                   (nix-package-menu--generate t t)) "-A" install-list)))
    ;; Delete packages, prompting if necessary.
    (when delete-list
      (if (or
           noquery
           (yes-or-no-p
           (if (= (length delete-list) 1)
               (format "Delete package `%s'? " (car delete-list))
             (format "Delete these %d packages (%s)? "
        	     (length delete-list)
        	     (intercalate delete-list ", ")))))
          (progn
            (apply #'nix-package-uninstall "-A" delete-list)
            (nix-package-menu--generate t t))
        (error "Aborted")))
    (unless (or delete-list install-list)
      (message "No operations specified."))))

(defun nix-package-menu--generate (remember-pos packages &optional keywords)
  "Populate the Package Menu.
 If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (nix-package-menu--refresh packages keywords)
  (setf (car (aref tabulated-list-format 0))
        (if keywords
            (let ((filters (mapconcat 'identity keywords ",")))
              (concat "Package[" filters "]"))
          "Package"))
  ;; (if keywords
  ;;     (define-key nix-package-menu-mode-map "q" 'package-show-package-list)
  ;;   (define-key nix-package-menu-mode-map "q" 'quit-window))
  (tabulated-list-init-header)
  (tabulated-list-print remember-pos))

(defface nix-package-available-package ()
  "The face used for packages that are available on the current platform."
  :group 'nix-package)

(defface nix-package-installed-package ()
  "The face used for packages that are available on the current platform."
  :group 'nix-package)

(defface nix-package--package ()
  "The face used for packages that are available on the current platform."
  :group 'nix-package)

;;; FIXME: Statuses should be something like:
;;; - I, installed
;;; - P, present (already on system, just link it)
;;; - S, substitutable (there’s one in the cache?)
;;; - _, buildable (gonna have to do it yourself)
;;; - D, disabled (not on the current platform, should be able to filter this)
;;;
;;; and we need a column for obsolete (<) / upgrade (>) indicators, and we
;;; should be able to filter the former
;;;
;;; use something like: nix-env -qasc --system --xml --description

(defun nix-package-menu--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (cdr (assoc 'id pkg)))
	 (status (cdr (assoc 'status pkg)))
	 (face (pcase (subseq status 0 1)
                 (`"I" 'font-lock-variable-name-face)
                 (`"P" 'font-lock-constant-face)
                 (`"S" 'bold)
                 (`" " 'default)
                 (`"D" 'shadow)))
         (name-sections (split-string (cdr (assoc 'name pkg)) "-")))
    (destructuring-bind (channel . group-sections)
        (split-string pkg-desc "\\.")
      (destructuring-bind (name version)
          (if (<= (length name-sections) 1)
              (list (car name-sections) "")
            (list (intercalate (butlast name-sections) "-")
                  (car (last name-sections))))
        (list (intern pkg-desc)
              `[,(list name
                       'face 'link
                       'follow-link t
                       'package-desc (intern pkg-desc)
                       'action 'nix-package-menu-describe-package)
                ,(propertize (or (cdr (assoc 'version (cdr (assoc 'meta (cdr pkg)))))
                                 version)
                             'font-lock-face face)
                ,(propertize channel 'font-lock-face face)
                ,(propertize (intercalate (butlast group-sections) ".") 'font-lock-face face)
                ,(propertize status 'font-lock-face face)
                ,(propertize (replace-regexp-in-string
                              "\n" " "
                              (or (cdr (assoc 'description pkg)) ""))
                             'font-lock-face face)])))))

(provide 'nix-package)
