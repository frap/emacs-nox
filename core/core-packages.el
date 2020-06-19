;;; core/core-packages.el -*- lexical-binding: t; -*-

;; Emacs package management is opinionated, and so is Doom. Doom uses `straight'
;; to create a declarative, lazy-loaded and (nominally) reproducible package
;; management system. We use `straight' over `package' because the latter is
;; tempermental. ELPA sources suffer downtime occasionally and often fail to
;; build packages when GNU Tar is unavailable (e.g. MacOS users start with BSD
;; tar). Known gnutls errors plague the current stable release of Emacs (26.x)
;; which bork TLS handshakes with ELPA repos (mainly gnu.elpa.org). See
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=3434.\\

;;
;;; Package managers

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quick package testing.
(setq package-enable-at-startup nil
      package-user-dir (concat enfer-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      ;; I omit Marmalade because its packages are manually submitted rather
      ;; than pulled, so packages are often out of date with upstream.
      package-archives
      (let ((proto (if gnutls-verify-error "https" "http")))
        `(("gnu"   . ,(concat proto "://elpa.gnu.org/packages/"))
          ("melpa" . ,(concat proto "://melpa.org/packages/"))
          ("org"   . ,(concat proto "://orgmode.org/elpa/")))))

;; package.el has no business modifying the user's init.el
(advice-add #'package--ensure-init-file :override #'ignore)

;; Refresh package.el the first time you call `package-install', so it can still
;; be used (e.g. to temporarily test packages). Remember to run 'doom sync' to
;; purge them; they can conflict with packages installed via straight!
(add-transient-hook! 'package-install (package-refresh-contents))


;;
;;; Straight

;; Setup straight.el
(setq straight-base-dir enfer-local-dir
      straight-build-dir (concat straight-base-dir "straight/build")
      straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-vc-git-default-clone-depth     1
  ;;    straight-enable-use-package-integration nil
      straight-check-for-modifications        '(find-when-checking))

;; create directories if they dont exist
(dolist (dir (list enfer-local-dir enfer-etc-dir enfer-cache-dir straight-build-dir))
  (unless (file-directory-p dir) (make-directory dir t)))

(defvar bootstrap-version)
(let* ((user-emacs-directory straight-base-dir)
       (bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
       (bootstrap-version 5))
  (make-directory (concat straight-base-dir "straight/build") 'parents)
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;; Setup use-package.el
(setq package-enable-at-startup nil)
;;;; use-package

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(use-package use-package-chords :ensure
:config (key-chord-mode t))

(defvar doom-deferred-packages-alist '(t))
(after! use-package-core
  (add-to-list 'use-package-deferring-keywords :after-call nil #'eq)
  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))

  (defalias 'use-package-normalize/:after-call 'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (intern (format "doom|transient-hook--load-%s" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (when doom-debug-mode
                     (message "Chargement du colis différé %s de %s" ',name ',fn))
                   (condition-case e (require ',name)
                     ((debug error)
                      (message "N'a pas réussi à charger le paquet différé %s: %s" ',name e)))
                   (dolist (hook (cdr (assq ',name doom-deferred-packages-alist)))
                     (if (functionp hook)
                         (advice-remove hook #',fn)
                       (remove-hook hook #',fn)))
                   (delq (assq ',name doom-deferred-packages-alist)
                         doom-deferred-packages-alist)
                   (fmakunbound ',fn))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (functionp hook)
                       `(advice-add #',hook :before #',fn)
                     `(add-hook ',hook #',fn))
                   forms)))
         `((unless (assq ',name doom-deferred-packages-alist)
             (push '(,name) doom-deferred-packages-alist))
           (nconc (assq ',name doom-deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))

;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)


;; Garbage Collector Magical Hack - to adjust garbade collection
(straight-use-package 'gcmh)
(use-package gcmh
  ;;:demand t
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold #x800000
        gcmh-high-cons-threshold most-positive-fixnum
        gcmh-idle-delay          3600)
  :config
  (gcmh-mode))

(use-package async
  :demand
  :config
  (async-bytecomp-package-mode)
  (setq! async-bytecomp-allowed-packages '(all)))

(provide 'core-packages)
;;; core-packages.el ends here
