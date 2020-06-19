;; -*- lexical-binding: t -*-

;; Measure startup time
(require 'init-benchmarking)

(setq-default user-full-name "Andrés Gasson")

(require 'subr-x)
(require 'cl-lib)

(defvar gas-debug-mode (when (or (getenv "DEBUG") init-file-debug) t)
  "If non-nil, all functions will be verbose. Set DEBUG=1 in the command
line or use --debug-init to enable this.")

(defconst EMACS26+ (eval-when-compile (not (version< emacs-version "26"))))
(defconst EMACS27+ (eval-when-compile (not (version< emacs-version "27"))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defconst gas-emacs-dir (eval-when-compile (file-truename user-emacs-directory))
  "The path to the emacs.d directory. Must end in a slash.")

(defconst gas-local-dir (concat gas-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defconst gas-etc-dir (concat gas-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries, external
dependencies or long-term shared data.")

(defconst gas-cache-dir (concat gas-local-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files.")

(defconst gas-packages-dir (concat gas-local-dir "packages/")
  "Where packages are stored.")

(defconst gas-develop-dir  "~/Dev"
  "Where gas development goes on.")

(dolist (dir (list gas-local-dir gas-etc-dir gas-cache-dir gas-packages-dir))
  (unless (file-directory-p dir) (make-directory dir t)))

(defmacro setq! (&rest settings)
  "Like setq-default, but uses custom-set if it is set."
  `(progn ,@(cl-loop for (var val) on settings by 'cddr
                     collect `(funcall (or (get ',var 'custom-set) #'set-default)
                       ',var ,val))))

(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defalias 'lambda! 'λ!)

(defun gas-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun gas-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun gas-pos-at-line (line &optional column)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (move-to-column (or column 0))
    (point)))

(defun gas-get-line-from-file (file line &optional trim)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (forward-line (- line 1))
      (let ((string (thing-at-point 'line)))
        (if trim
            (replace-regexp-in-string "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)" "" string)
          string)))))

(defun gas-get-entire-buffer (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max))))
)

(defun +color-name-lighter-than (a b)
  (> (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb a)))
     (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb b))))
)

(defun gas-keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str))
)

(defun gas-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type :test keyword)
  (substring (symbol-name keyword) 1)
)

(defun gas-resolve-hook-forms (hooks)
  (declare (pure t) (side-effect-free t))
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (gas-enlist (gas-unquote hooks))
           if (eq (car-safe hook) 'quote)
           collect (cadr hook)
           else if quoted-p
           collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

   1. Optional properties :local and/or :append, which will make the hook
      buffer-local or append to the list of hooks (respectively),
   2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
      a quoted hook variable or a quoted list of hook variables. If unquoted, the
      hooks will be resolved by appending -hook to each symbol.
   3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)   (same as `add-hook')
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable `args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (gas-resolve-hook-forms (pop args)))
          (funcs (let ((arg (car args)))
                   (if (memq (car-safe arg) '(quote function))
                       (if (cdr-safe (cadr arg))
                           (cadr arg)
                         (list (cadr arg)))
                     (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(if append-p (nreverse forms) forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as `add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! :remove ,@args))

(defmacro setq-hook! (hooks &rest rest)
  "Convenience macro for setting buffer-local variables in a hook.

  (setq-hook! 'markdown-mode-hook
    line-spacing 2
    fill-column 80)"
  (declare (indent 1))
  (unless (= 0 (% (length rest) 2))
    (signal 'wrong-number-of-arguments (length rest)))
  `(add-hook! ,hooks
     ,@(let (forms)
         (while rest
           (let ((var (pop rest))
                 (val (pop rest)))
             (push `(setq-local ,var ,val) forms)))
         (nreverse forms))))

(defmacro add-transient-hook! (hook-or-function &rest args)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised).

ARGS can be a function, list of functions, or body forms to be wrapped in a lambda.
When it is a function or a list of functions, they will be called with the hooks args."
  (declare (indent 1))
  (let ((append (if (eq (car args) :after) (pop args)))
        ;; NOTE(gas):
        ;; If args is a function or list of functions, funcs will be a list of functions
        ;; If args is a list of forms, funcs will be a list containing only the list of forms
        (funcs (let ((arg (car args)))
                 (if (memq (car-safe arg) '(quote function))
                     (if (cdr-safe (cadr arg))
                         (cadr arg)
                       (list (cadr arg)))
                   (list args))))
        (func-name (gensym "gas|transient-hook-")))
    `(progn
       (fset ',func-name
             (lambda (&rest call-args)
               ,@(cl-loop for fn in funcs
                          collect (if (symbolp fn)
                                      `(apply #',fn call-args)
                                    `(progn ,@args)))
               (cond ((functionp ,hook-or-function) (advice-remove ,hook-or-function #',func-name))
                     ((symbolp ,hook-or-function)   (remove-hook ,hook-or-function #',func-name)))
               (unintern ',func-name nil)))
       (cond ((functionp ,hook-or-function)
              (advice-add ,hook-or-function ,(if append :after :before) #',func-name))
             ((symbolp ,hook-or-function)
              (put ',func-name 'permanent-local-hook t)
              (add-hook ,hook-or-function #',func-name ,append))))))

(defmacro after! (targets &rest body)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation. This will no-op on features that have been disabled by the user."
  (declare (indent defun) (debug t))
  (list (if (or (not (bound-and-true-p byte-compile-current-file))
                (dolist (next (gas-enlist targets))
                  (unless (keywordp next)
                    (if (symbolp next)
                        (require next nil :no-error)
                      (load next :no-message :no-error)))))
            #'progn
          #'with-no-warnings)
        (if (symbolp targets)
            `(with-eval-after-load ',targets ,@body)
          (pcase (car-safe targets)
            ((or :or :any)
             (macroexp-progn
              (cl-loop for next in (cdr targets)
                       collect `(after! ,next ,@body))))
            ((or :and :all)
             (dolist (next (cdr targets))
               (setq body `((after! ,next ,@body))))
             (car body))
            (_ `(after! (:and ,@targets) ,@body))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any output."
  `(if gas-debug-mode
       (progn ,@forms)
     (let ((old-fn (symbol-function 'write-region)))
       (cl-letf* ((standard-output (lambda (&rest _)))
                  ((symbol-function 'load-file) (lambda (file) (load file nil t)))
                  ((symbol-function 'message) (lambda (&rest _)))
                  ((symbol-function 'write-region)
                   (lambda (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall old-fn start end filename append visit lockname mustbenew)))
                  (inhibit-message t)
                  (save-silently t))
         ,@forms))))

(defun gas*shut-up (orig-fn &rest args)
  "Generic advisor for silencing noisy functions."
  (quiet! (apply orig-fn args)))

(defun gas/rename-file-and-buffer ()
  "Rename current buffer and the file it is visiting, if any."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t))
        (set-visited-file-name new-name t t)))))

(defun gas/delete-file-and-buffer ()
  "Kill the current buffer and delete the file it is visiting, if any."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun gas/previous-blank-line ()
  "Move point to the previous blank line"
  (interactive)
  (move-end-of-line nil)
  (if (search-backward-regexp "^[\t ]*\n[\t ]*[^\t\n ]+" nil "NOERROR") nil
    (goto-char (point-min))))

(defun gas/next-blank-line ()
  "Move point to the next blank line"
  (interactive)
  (move-beginning-of-line nil)
  (if (not (search-forward-regexp "[^\t\n ]\n[\t ]*$" nil "NOERROR"))
      (goto-char (point-max))))

(defun gas/open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line 0)
  (newline-and-indent))

(defun gas/open-line-below ()
  "Insert an empty line below the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun gas/exit-emacs (arg)
  "Exit Emacs, possibly killing the daemon and/or saving buffer.
When ARG is:
- nil or negative, it will kill the current terminal
- `universal-argument' or positive, it will kill the daemon
- a number, it will save all buffers automatically"
  (interactive "P")
  (when (or (numberp arg) (eq arg '-))
    (setq arg (prefix-numeric-value arg)))
  (let* ((save-without-asking (numberp arg))
         (kill-server (or (equal arg '(4))
                          (and save-without-asking
                               (>= arg 0)))))
    (if kill-server
        (save-buffers-kill-emacs save-without-asking)
      (save-buffers-kill-terminal save-without-asking))))

(defmacro gas-measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun gas-byte-compile-init ()
  (byte-compile-file (concat user-emacs-directory "config.el"))
  (byte-compile-file (concat user-emacs-directory "init.el"))
  (byte-compile-file (concat user-emacs-directory "early-init.el")))

(defvar gas-exit-window-hook nil
  "Hook run before `switch-window' or `switch-frame' are called. See
`gas-enter-window-hook'.")

(defvar gas-enter-window-hook nil
  "Hook run after `switch-window' or `switch-frame' are called. See
`gas-exit-window-hook'.")

(defvar gas-exit-buffer-hook nil
  "Hook run before `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.")

(defvar gas-enter-buffer-hook nil
  "Hook run after `switch-to-buffer', `pop-to-buffer' or `display-buffer' are
called. The buffer to be switched to is current when these hooks run.")

(defvar gas-inhibit-switch-buffer-hooks nil)
(defvar gas-inhibit-switch-window-hooks nil)

(defun gas*switch-window-hooks (orig-fn window &optional norecord)
  (if (or gas-inhibit-switch-window-hooks
          (null window)
          (eq window (selected-window))
          (window-minibuffer-p)
          (window-minibuffer-p window))
      (funcall orig-fn window norecord)
    (let ((gas-inhibit-switch-window-hooks t))
      (run-hooks 'gas-exit-window-hook)
      (prog1 (funcall orig-fn window norecord)
        (with-selected-window window
          (run-hooks 'gas-enter-window-hook))))))

(defun gas*switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (if (or gas-inhibit-switch-buffer-hooks
          (eq (get-buffer buffer-or-name) (current-buffer)))
      (apply orig-fn buffer-or-name args)
    (let ((gas-inhibit-switch-buffer-hooks t))
      (run-hooks 'gas-exit-buffer-hook)
      (prog1 (apply orig-fn buffer-or-name args)
        (with-current-buffer buffer-or-name
          (run-hooks 'gas-enter-buffer-hook))))))

(defun gas|setup-switch-hooks (&optional disable)
  (dolist (spec '((select-window . gas*switch-window-hooks)
                  (switch-to-buffer . gas*switch-buffer-hooks)
                  (display-buffer . gas*switch-buffer-hooks)
                  (pop-to-buffer . gas*switch-buffer-hooks)))
    (if disable
        (advice-remove (car spec) (cdr spec))
      (advice-add (car spec) :around (cdr spec)))))

;; disabling as not sure how works
(add-hook 'emacs-startup-hook #'gas|setup-switch-hooks)

(eval-and-compile
  (unless EMACS26+
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let)

      ;; `alist-get' doesn't have its 5th argument before Emacs 26
      (defun gas*alist-get (key alist &optional default remove testfn)
        (ignore remove)
        (let ((x (if (not testfn)
                     (assq key alist)
                   (assoc key alist testfn))))
          (if x (cdr x) default)))
      (advice-add #'alist-get :override #'gas*alist-get)
      (defvar gensym-counter 0
 	  "Number used to construct the name of the next symbol created by `gensym'.")

      (defun gensym (&optional prefix)
        "Return a new uninterned symbol.
 	The name is made by appending `gensym-counter' to PREFIX.
 	PREFIX is a string, and defaults to \"g\"."
        (let ((num (prog1 gensym-counter
                     (setq gensym-counter (1+ gensym-counter)))))
          (make-symbol (format "%s%d" (or prefix "g") num))))
      (defun cadar (x)
 	      "Return the `car' of the `cdr' of the `car' of X."
 	      (declare (compiler-macro internal--compiler-macro-cXXr))
 	       (car (cdr (car x))))
)))

;; Setup straight.el
(setq straight-base-dir gas-local-dir
      straight-build-dir (concat straight-base-dir "straight/build")
      straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-vc-git-default-clone-depth     1
  ;;    straight-enable-use-package-integration nil
      straight-check-for-modifications        '(find-when-checking))

;; create directories if they dont exist
(dolist (dir (list gas-local-dir gas-etc-dir gas-cache-dir straight-build-dir))
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

;;; key bindings
(use-package bind-key
 :disabled
  ;; A simple way to manage personal keybindings, provided by `use-package'
  :init
  ;; If non-nil, extract docstrings from lambdas, closures and keymaps if possible.
  (setq bind-key-describe-special-forms t)

  (defun my-keyboard-translations (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (define-key input-decode-map (kbd "C-h") (kbd "<backspace>"))
      (define-key input-decode-map (kbd "M-h") (kbd "<M-backspace>"))))
  (add-to-list 'after-make-frame-functions 'my-keyboard-translations)
  )

;(setq use-package-always-ensure t)

(use-package use-package-chords :ensure
:config (key-chord-mode t))


(defvar gas-deferred-packages-alist '(t))
(after! use-package-core
  (add-to-list 'use-package-deferring-keywords :after-call nil #'eq)
  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))

  (defalias 'use-package-normalize/:after-call 'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (intern (format "gas|transient-hook--load-%s" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (when gas-debug-mode
                     (message "Chargement du colis différé %s de %s" ',name ',fn))
                   (condition-case e (require ',name)
                     ((debug error)
                      (message "N'a pas réussi à charger le paquet différé %s: %s" ',name e)))
                   (dolist (hook (cdr (assq ',name gas-deferred-packages-alist)))
                     (if (functionp hook)
                         (advice-remove hook #',fn)
                       (remove-hook hook #',fn)))
                   (delq (assq ',name gas-deferred-packages-alist)
                         gas-deferred-packages-alist)
                   (fmakunbound ',fn))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (functionp hook)
                       `(advice-add #',hook :before #',fn)
                     `(add-hook ',hook #',fn))
                   forms)))
         `((unless (assq ',name gas-deferred-packages-alist)
             (push '(,name) gas-deferred-packages-alist))
           (nconc (assq ',name gas-deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))


(use-package async
  :demand
  :config
  (async-bytecomp-package-mode)
  (setq! async-bytecomp-allowed-packages '(all)))

(use-package hydra
  :bind ("C-c m" . hydra-magit/body)
  :config (setq! lv-use-separator t))

(defhydra hydra-zoom (global-map "C-c")
  "zoom"
  ("+" default-text-scale-increase "in")
  ("=" default-text-scale-increase "in")
  ("-" default-text-scale-decrease "out"))

(setq!
 ad-redefinition-action 'accept
 auto-window-vscroll nil ;; https://emacs.stackexchange.com/a/28746
 autoload-compute-prefixes nil
 bidi-display-reordering nil
 byte-compile-verbose gas-debug-mode
 debug-on-error gas-debug-mode
 ffap-machine-p-known 'reject
 idle-update-delay 2
 inhibit-compacting-font-caches t
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(prefer-coding-system        'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-default-coding-systems  'utf-8-unix)

(setq! inhibit-startup-message t
       inhibit-startup-echo-area-message user-login-name
       inhibit-default-init t
       initial-major-mode 'fundamental-mode
       initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

(defun gas*server-execute-quiet (orig-fn &rest args)
  "Shup ut `server-execute' once."
  (quiet! (apply orig-fn args))
  (advice-remove 'server-execute 'gas*server-execute-quiet))
(when (daemonp)
  (advice-add 'server-execute :around 'gas*server-execute-quiet))

(setq! abbrev-file-name               (concat gas-local-dir "abbrev.el")
       cider-history                  (concat gas-local-dir "cider-history")
       auto-save-file-name-transforms (list (list ".*" (concat gas-cache-dir "auto-save/") t))
       auto-save-list-file-prefix     (concat gas-cache-dir "auto-save/.saves-")
       auto-save-list-file-name       (concat gas-cache-dir "auto-save-list")
       backup-directory-alist         (list (cons "." (concat gas-cache-dir "backup/")))
       custom-file                    (concat gas-local-dir "custom.el")
       mc/list-file                   (concat gas-etc-dir "mc-lists.el")
       pcache-directory               (concat gas-cache-dir "pcache/")
       request-storage-directory      (concat gas-cache-dir "request")
       server-auth-dir                (concat gas-cache-dir "server/")
       shared-game-score-directory    (concat gas-etc-dir "shared-game-score/")
       url-cache-directory            (concat gas-cache-dir "url/")
       url-configuration-directory    (concat gas-etc-dir "url/"))

(make-directory (cadar auto-save-file-name-transforms) t)

(setq! delete-by-moving-to-trash t
       delete-old-versions t
       history-length 500
       kept-new-versions 10
       kept-old-versions 2
       version-control t)

(setq! gnutls-verify-error (not (getenv "INSECURE"))
       tls-checktrust gnutls-verify-error
       tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                         "gnutls-cli -p %p %h"
                         "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(defun gas*set-indirect-buffer-filename (orig-fn base-buffer name &optional clone)
  "In indirect buffers, `buffer-file-name' is nil, which can cause problems
with functions that require it."
  (let ((file-name (buffer-file-name base-buffer))
        (buffer (funcall orig-fn base-buffer name clone)))
    (when (and file-name buffer)
      (with-current-buffer buffer
        (unless buffer-file-name
          (setq buffer-file-name file-name
                buffer-file-truename (file-truename file-name)))))
    buffer))
(advice-add #'make-indirect-buffer :around #'gas*set-indirect-buffer-filename)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq! x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
       select-enable-clipboard t
       select-enable-primary t)

(defmacro set-env! (&rest _vars)
  "Inject VARS from your shell environment into Emacs.")

(cond (IS-MAC
       (setq! mac-command-modifier 'super
              mac-option-modifier  'meta
              mac-right-option-modifier 'alt
             ;; sane trackpad/mouse scroll settings
             mac-redisplay-dont-reset-vscroll t
             mac-mouse-wheel-smooth-scroll nil
             mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
             mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
             ;; Curse Lion and its sudden but inevitable fullscreen mode!
             ;; NOTE Meaningless to railwaycat's emacs-mac build
             ns-use-native-fullscreen nil
             ;; Visit files opened outside of Emacs in existing frame, rather
             ;; than a new one
             ns-pop-up-frames nil)

       ;; Fix the clipboard in terminal or daemon Emacs (non-GUI)
       (when (or (daemonp) (not (display-graphic-p)))
         (add-hook 'emacs-startup-hook #'osx-clipboard-mode))

       (when (or (daemonp) (display-graphic-p))
         ;; Syncs ns frame parameters with theme (and fixes mismatching text
         ;; colr in the frame title)
         (when (require 'ns-auto-titlebar nil t)
           (add-hook 'doom-load-theme-hook #'ns-auto-titlebar-mode))

         ;; A known problem with GUI Emacs on MacOS (or daemons started via
         ;; launchctl or brew services): it runs in an isolated
         ;; environment, so envvars will be wrong. That includes the PATH
         ;; Emacs picks up. `exec-path-from-shell' fixes this.
         (when (require 'exec-path-from-shell nil t)
           (defun set-env! (&rest vars)
             "Inject VARS from your shell environment into Emacs."
             (exec-path-from-shell-copy-envs vars))
           (setq! exec-path-from-shell-check-startup-files nil
                 exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments)
                 exec-path-from-shell-debug gas-debug-mode
                 exec-path-from-shell-variables
                 (nconc exec-path-from-shell-variables '("LC_CTYPE" "LC_ALL" "LANG")))
           (exec-path-from-shell-initialize))))

      (IS-LINUX
       (setq! x-gtk-use-system-tooltips nil    ; native tooltips are ugly!
             x-underline-at-descent-line t))  ; draw underline lower

      )

(setq! ring-bell-function #'ignore
       visible-bell nil
       custom-safe-themes t
       frame-inhibit-implied-resize t
       mode-line-default-help-echo nil
       use-dialog-box nil
        pos-tip-internal-border-width 6
       pos-tip-border-width 1
    ;;   window-resize-pixelwise t
       frame-resize-pixelwise t
       echo-keystrokes 0.2
       window-divider-default-places t
       window-divider-default-bottom-width 1
       window-divider-default-right-width 1
       frame-title-format '("%b - Gas Emacs")
       tooltip-hide-delay 3600)

(fset 'yes-or-no-p 'y-or-n-p)
(minibuffer-depth-indicate-mode)

(setq! cursor-in-non-selected-windows t
       highlight-nonselected-windows nil
       visible-cursor nil
       x-stretch-cursor nil)

(blink-cursor-mode -1)

(window-divider-mode)

(setq! jit-lock-defer-time nil
       jit-lock-stealth-nice 0.1
       jit-lock-stealth-time 0.2
       jit-lock-stealth-verbose nil)

(use-package hl-line
  :hook ('emacs-startup-hook #'global-hl-line-mode)
  :config
  (setq! hl-line-sticky-flag nil
         global-hl-line-sticky-flag nil))

(use-package paren
  :after-call (after-find-file gas-before-switch-buffer-hook)
  :config
  (setq show-paren-delay 0
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode))

(unless (fboundp 'define-fringe-bitmap) (defun define-fringe-bitmap (&rest _)))

(use-package server
  :config
  (add-hook 'after-make-frame-functions (lambda (frame) (select-frame-set-input-focus frame)) t)

  ;; Remove prompt if the file is opened in other clients
  (defun server-remove-kill-buffer-hook ()
    (remove-hook 'kill-buffer-query-functions #'server-kill-buffer-query-function))
  (add-hook 'server-visit-hook #'server-remove-kill-buffer-hook))

(defvar gas-fonts '(("Hack" . 13)("Iosevka" . 13) ("DejaVu Sans Mono" . 11)
                    ("Inconsolata" . 13) ("Source Code Pro" . 11))
  "List of fonts and sizes. The first one available will be used.")

(defvar gas-font-faces-changed nil
  "List ARGS passed to custom-set-faces, in order to fix font.")

(defun gas-font-set-faces (&rest args)
  "Override faces' attributes in the `user' theme.
These settings will remain until a new font is loaded.
ARGS are the same as in `custom-set-faces'."
  (push args gas-font-faces-changed)
  (apply 'custom-set-faces args))

(defun gas/change-font ()
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font gas-fonts)
      (when (member (car font) (font-family-list))
        (push font available-fonts)))

    (push (cons "Monospace" 11) available-fonts)
    (setq available-fonts (nreverse available-fonts))

    (when gas-debug-mode (message "polcies disponible: %s" available-fonts))

    (if (called-interactively-p 'interactive)
        (let* ((chosen (assoc-string (completing-read "Quelle police utiliser? " available-fonts nil t)
                                     available-fonts)))
          (setq font-name (car chosen)
                font-size (read-number "Taille de police: " (cdr chosen))))
      (setq font-name (caar available-fonts)
            font-size (cdar available-fonts)))

    (setq font-setting (format "%s-%d" font-name font-size))
    (set-frame-font font-setting nil t)
    (add-to-list 'default-frame-alist (cons 'font font-setting))

    (dolist (args gas-font-faces-changed)
      (apply 'custom-theme-reset-faces 'user args))
    (setq gas-font-faces-changed nil)

    (cond ((string= font-name "Hack")
           (gas-font-set-faces `(org-table ((t (:family ,(format "Hack Mono-%d" font-size))))))))))

(defvar gas-customise-theme-hook nil
  "Hook for theme customisation, called with the theme name.")

(defvar gas-theme-faces-changed nil
  "List ARGS passed to custom-set-faces, in order to fix theme.")

(defun gas*customise-theme (theme)
  (unless (eq theme 'user)
    (dolist (enabled-theme custom-enabled-themes)
      (unless (eq enabled-theme theme) (disable-theme enabled-theme))))

  (dolist (args gas-theme-faces-changed)
    (apply 'custom-theme-reset-faces 'user args))
  (setq gas-theme-faces-changed nil)

  (run-hook-with-args-until-success 'gas-customise-theme-hook (or theme
                                                                  (car custom-enabled-themes))))
(advice-add 'enable-theme :after #'gas*customise-theme)

(defmacro gas-add-customise-theme-hook (target-theme &rest body)
  "TARGET-THEME may be a list, a symbol or a regexp."
  (declare (indent defun))
  `(add-hook 'gas-customise-theme-hook
             (lambda (theme)
               ,(cond ((symbolp (eval target-theme))
                       `(when (eq theme ,target-theme) ,@body))
                      ((stringp (eval target-theme))
                       `(when (string-match ,target-theme (symbol-name theme)) ,@body))
                      ((listp (eval target-theme))
                       `(when (memq theme ,target-theme) ,@body))))))

(defun gas-theme-set-faces (&rest args)
  "Override faces' attributes in the `user' theme.
These settings will remain until a new theme is loaded.
ARGS are the same as in `custom-set-faces'."
  (push args gas-theme-faces-changed)
  (apply 'custom-set-faces args))

(use-package doom-themes
  :config
  (setq! doom-one-brighter-comments t
         doom-one-comment-bg nil)

  (after! org (doom-themes-org-config))

  (gas-add-customise-theme-hook "^doom-"
    (gas-theme-set-faces '(org-level-1 ((t (:height 1.0))))
                         '(org-level-2 ((t (:height 1.0))))
                         '(org-level-3 ((t (:height 1.0)))))
    (custom-theme-set-faces
     theme
     '(org-special-keyword ((t (:inherit shadow))))
     '(git-commit-overlong-summary ((t (:inherit shadow)))))))

(use-package color-theme-sanityinc-tomorrow
  :config
  (gas-add-customise-theme-hook "^sanityinc-"
    (custom-theme-set-faces
     theme
     `(org-special-keyword ((t (:inherit shadow)))))))

(use-package gruvbox-theme
  :defer t)

(defface font-lock-todo-face      '((t (:foreground "#dc322f" :weight bold :underline t))) "Face for TODO keywords.")
(defface font-lock-urgent-face    '((t (:foreground "#b58900" :weight bold :underline t))) "Face for URGENT keywords.")
(defface font-lock-note-face      '((t (:foreground "#228b22" :weight bold :underline t))) "Face for NOTE keywords.")
(defface font-lock-study-face     '((t (:foreground "#41728e" :weight bold :underline t))) "Face for STUDY keywords.")
(add-hook! prog-mode (font-lock-add-keywords
                      nil '(("\\<\\(TODO\\|FIXME\\|\\@important\\|IMPORTANT\\)" 1 'font-lock-todo-face t)
                            ("\\<\\(URGENT\\|\\@urgent\\)" 1 'font-lock-urgent-face t)
                            ("\\<\\(NOTE\\|QUOTE\\)" 1 'font-lock-note-face t)
                            ("\\<\\(STUDY\\|LAREVUE\\)" 1 'font-lock-study-face t))))

(use-package all-the-icons
  :init
  (defun gas*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                all-the-icons-faicon all-the-icons-fileicon
                all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'gas*disable-all-the-icons-in-tty)))

(defun gas|no-fringes-in-minibuffer (&rest _)
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(emacs-startup-hook minibuffer-setup-hook window-configuration-change-hook)
           #'gas|no-fringes-in-minibuffer)

(use-package doom-modeline
  :config
  (setq! doom-modeline-enable-word-count t))

(setq!
 mode-line-buffer-identification
 `((:eval
    (let ((inherit-faces (if (and buffer-file-name (buffer-modified-p)) '(error bold) 'mode-line-buffer-id)))
      (concat (propertize "%b" 'face inherit-faces)))))
 mode-line-modes '("" mode-name mode-line-process " ")
 mode-line-position
 '((line-number-mode (column-number-mode (column-number-indicator-zero-based " %l:%c" " %l:%C") " %l")
                     (column-number-mode (column-number-indicator-zero-based " :%c" " :%C")))
   (" " mode-line-percent-position " ")))

(defun gas-setup-appearance (frame)
  (with-selected-frame frame
    ;;(load-theme 'doom-city-lights t)
    (load-theme 'doom-monokai-pro t)
    ;;(load-theme gruvbox t)
    (gas/change-font)

    (doom-modeline-mode)
    (when (> (window-width) 100) (split-window-right))
    (setq! system-time-locale "C")))

(if (daemonp)
    (add-transient-hook! 'after-make-frame-functions 'gas-setup-appearance)
  (add-transient-hook! 'emacs-startup-hook (gas-setup-appearance (selected-frame)))
)

(use-package shackle
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.4) ; default 0.5

    (setq shackle-rules
          ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
          '((compilation-mode              :select nil                                               )
            ("*undo-tree*"                                                    :size 0.25 :align right)
            ("*eshell*"                    :select t                          :other t               )
            ("*HTTP Response*"             :popup t                                      :align right)
            ("*Shell Command Output*"      :select nil                                               )
            ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
            (occur-mode                    :select nil                                   :align t    )
           ;; (cider-repl-mode               :select nil                                   :popup t    )

            ("*Help*"                      :select t   :inhibit-window-quit t :other t               )
            ("*Completions*"                                                  :size 0.3  :align t    )
            ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
            ("\\*[Wo]*Man.*\\*"    :regexp t :select t   :inhibit-window-quit t :other t               )
            ("\\*poporg.*\\*"      :regexp t :select t                          :other t               )
            ("\\`\\*helm.*?\\*\\'"   :regexp t                                    :size 0.3  :align t    )
            ("*Calendar*"                  :select t                          :size 0.3  :align below)
            ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
            (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
            (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
            ))

    (shackle-mode 1)))

(use-package ido
  :disabled t
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1))

(use-package ido-completing-read+
  :disabled t
  :config
  (ido-ubiquitous-mode 1))

(use-package amx :ensure
 :disabled t
 :defer 0.5
  :config (amx-mode)
  :custom
  (amx-save-file (concat gas-cache-dir "amx-items")))

(use-package ivy
  :disabled t
 ;; :bind (("C-c u" . ivy-resume))
  :blackout ivy-mode
  :defer 0.5
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-initial-inputs-alist nil
        ivy-virtual-abbreviate 'full ; Show the full virtual file paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-format-function 'ivy-format-function-arrow
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-selectable-prompt t)

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press)))

(use-package ivy-rich
  :disabled t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :disabled t
  :after ivy
  :bind (("C-r" . swiper)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c C-r" . ivy-resume)
         ("C-S-s" . isearch-forward)
         ("C-x b" . ivy-switch-buffer)
         ("C-c u" . swiper-all))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t))

(use-package flx :ensure
  :disabled t
  :after ivy
  :demand
  :config
  (setq! ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
                                 (counsel-rg . ivy--regex-plus)
                                 (counsel-pt . ivy--regex-plus)
                                 (counsel-grep . ivy--regex-plus)
                                 (swiper . ivy--regex-plus)
                                 (t . ivy--regex-fuzzy))))

(require 'auth-source)
(require 'auth-source-pass)
(auth-source-pass-enable)

(setq! vc-follow-symlinks t
       save-interprogram-paste-before-kill t
       enable-recursive-minibuffers t
       mouse-yank-at-point t)

 (add-hook 'before-save-hook 'time-stamp)

(setq! tab-width 4
       indent-tabs-mode nil
       require-final-newline t
       mode-require-final-newline t
       sentence-end-double-space nil
       tab-always-indent t
       tabify-regexp "^\t* [ \t]+"
       fill-column 90
       word-wrap t
       truncate-lines t
       truncate-partial-width-windows 70)

(add-hook! 'before-save-hook #'delete-trailing-whitespace)
(add-hook! 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setq! scroll-margin 1
       hscroll-margin 2
       hscroll-step 1
       scroll-conservatively 101
       scroll-preserve-screen-position t
       mouse-wheel-scroll-amount '(1)
       mouse-wheel-progressive-speed nil)

(setq! kill-ring-max 5000
       undo-limit (* 20 1024 1024)
       undo-strong-limit (* 40 1024 1024)
       undo-outer-limit (* 100 1024 1024)
       mark-ring-max 5000
       global-mark-ring-max 5000)

(use-package autorevert
  :after-call after-find-file
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package recentf
  :defer 1
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat gas-cache-dir "recentf")
        recentf-auto-cleanup 120
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              (lambda (file) (file-in-directory-p file gas-local-dir))))
  (quiet! (recentf-mode)))

(use-package bookmark
  :custom
  (bookmark-default-file (concat gas-etc-dir "bookmarks"))
  (bookmark-save-flag t))

(use-package company
  :after-call (pre-command-hook after-find-file dired-before-readin-hook)
  :bind ("C-<tab>" . company-complete)  ; In case I don't want to wait
  :config
  (setq! company-idle-delay nil
         company-tooltip-limit 15
         company-tooltip-align-annotations t
         company-require-match 'never
         company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
         company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
         company-transformers '(company-sort-by-occurrence))

  (setq-hook! prog-mode
    company-backends '((:separate company-dabbrev-code company-capf company-yasnippet)))

  (setq-hook! text-mode
    company-backends '((:separate company-dabbrev company-capf company-yasnippet)))

  (global-company-mode))

(use-feature company-dabbrev
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-ignore-invisible t)
  (company-dabbrev-code-other-buffers t)

  :config
  (defun gas-company-dabbrev-buffer-check (buffer)
    (with-current-buffer buffer (derived-mode-p 'pdf-view-mode
                                                'doc-view-mode)))
  (setq! company-dabbrev-ignore-buffers #'gas-company-dabbrev-buffer-check))

(use-package company-box
 :defer
 :after company
 :hook (company-mode . company-box-mode))

(use-package projectile :ensure
  :after-call (pre-command-hook after-find-file dired-before-readin-hook)
  :commands (projectile-project-root projectile-project-name projectile-project-p)
  :init
  (setq! projectile-cache-file (concat gas-cache-dir "projectile.cache")
         projectile-indexing-method (if IS-WINDOWS 'native 'alien)
         projectile-known-projects-file (concat gas-cache-dir "projectile.projects")
         projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
         projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
         projectile-ignored-projects '("~" "/tmp"))

  :config
  ;; Set the projectile-prefix-command binding
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)

  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  (setq! projectile-globally-ignored-directories (append projectile-globally-ignored-directories
                                                         (list (abbreviate-file-name gas-local-dir)
                                                               ".sync" "node_modules" "flow-typed"))
         projectile-other-file-alist (append projectile-other-file-alist
                                             '(("css"  "scss" "sass" "less" "styl")
                                               ("scss" "css")
                                               ("sass" "css")
                                               ("less" "css")
                                               ("styl" "css"))))
  (push ".project" projectile-project-root-files-bottom-up)

  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)

  ;; NOTE(gas): Projectile root-searching functions can cause an infinite loop on TRAMP
  ;; connections, so disable them
  (defun gas*projectile-locate-dominating-file (orig-fn &rest args)
    "Don't traverse the file system if on a remote connection."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'projectile-locate-dominating-file :around #'gas*projectile-locate-dominating-file)

  (defun gas*projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (cl-loop for path in (projectile-ignored-directories)
                     if (string-prefix-p (or buffer-file-name "") (expand-file-name path))
                     return t)
      (apply orig-fun args)))
  (advice-add #'projectile-cache-current-file :around #'gas*projectile-cache-current-file)

  (projectile-mode))

 (use-package counsel-projectile :ensure
   :bind
     (([remap projectile-find-file]        .     counsel-projectile-find-file)
      ([remap projectile-find-dir]         .     counsel-projectile-find-dir)
      ([remap projectile-switch-to-buffer] .     counsel-projectile-switch-to-buffer)
      ([remap projectile-grep]             .     counsel-projectile-grep)
      ([remap projectile-ag]               .     counsel-projectile-ag)
      ([remap projectile-switch-project]   .     counsel-projectile-switch-project))

  :custom
  (projectile-completion-system 'ivy))

(use-package avy :ensure
  :bind
  ("C-:"   .   avy-goto-char)
  ("C-'"   .   avy-goto-char-timer)
  ("M-g f" .   avy-goto-line)
  ("M-g w" .   avy-goto-word-1)
  :config
  (setq! avy-all-windows nil
         avy-background t))

(use-package dumb-jump :ensure
  :commands dumb-jump-result-follow
  :bind
  ("M-g j" .    gas@dumb-jump/body)
  :config
  (defhydra gas@dumb-jump (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

  (setq dumb-jump-selector 'ivy
        dumb-jump-default-project gas-develop-dir
        dumb-jump-aggressive nil
        dumb-jump-use-visible-window nil
        dumb-jump-prefer-searcher 'rg)
)

(use-package imenu
  :custom
  (imenu-auto-rescan-maxout 500000)
  (imenu-auto-rescan t))

(use-package imenu-anywhere :ensure
  :init (global-set-key (kbd "C-.") 'imenu-anywhere)
  :custom
  (imenu-anywhere-delimiter ": ")
  :config (defun jcs-use-package ()
            (add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'jcs-use-package))

(add-hook!
  '(imenu-after-jump-hook
    evil-jumps-post-jump-hook
    counsel-grep-post-action-hook
    dumb-jump-after-jump-hook)
  #'recenter)

(use-package electric
  :init
  (electric-indent-mode))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package expand-region :ensure)

(setq large-file-warning-threshold (* 100 1024 1024))

(defvar gas-large-file-size 10
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar gas-large-file-modes-list
  '(fundamental-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode)
  "Major modes that `gas|check-large-file' will ignore.")

(defun gas|check-large-file ()
  "Check if the buffer's file is large (see `gas-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (when (and (not (memq major-mode gas-large-file-modes-list))
             auto-mode-alist
             (get-buffer-window))
    (when-let* ((size (nth 7 (file-attributes buffer-file-name))))
      (when (and (> size (* 1024 1024 gas-large-file-size))
                 (y-or-n-p
                  (format (concat "%s is a large file, open literally to "
                                  "avoid performance issues?")
                          (file-relative-name buffer-file-name))))
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode)))))
(add-hook 'find-file-hook #'gas|check-large-file)

;;;; prog-mode
;; Generic major mode for programming
(use-feature prog-mode
  :config
  (defun my-prog-mode-hook ()
    (setq show-trailing-whitespace 1)
    (prettify-symbols-mode 1))
  :hook (prog-mode . my-prog-mode-hook))

(use-package rust-mode
  :ensure
  :defer
  :mode ("\\.rs" . rust-mode)
)

;; a great lisp coding hook
(defun gas-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq gas-lisp-coding-hook 'gas-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun gas-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq gas-interactive-lisp-coding-hook 'gas-interactive-lisp-coding-defaults)

(use-feature lisp-mode
  :mode ("\\.lte" . lisp-mode)
  )

;(with-eval-after-load 'clojure-mode
;  (defun gas-clojure-mode-defaults ()
;    (subword-mode +1)
;    (run-hooks 'gas-lisp-coding-hook))

(use-package clojure-mode
  :ensure
  :defer
  :mode (("\\.edn$"  . clojurec-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         )
  :init

  :config
 (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)
      ("<=" . ?≤)
      (">=" . ?≥)
      ("<-" . ?←)
      ("->" . ?→)
      ("<=" . ?⇐)
      ("=>" . ?⇒)
      ("lambda" . ?λ)
      ))
  (add-hook! :append :local clojure-mode '(yas-minor-mode subword-mode global-prettify-symbols-mode gas-lisp-coding-hook))
)

(use-package eldoc
  :ensure
  :defer
  :hook
  (prog-mode       . turn-on-eldoc-mode)
  (cider-repl-mode . turn-on-eldoc-mode)
)

(defun zz/sp-enclose-next-sexp (num) (interactive "p") (insert-parentheses (or num 1)))
(use-package smartparens
  :ensure
  :defer
  :bind (("M-n" . sp-next-sexp)
         ("M-p" . sp-previous-sexp)
         ("M-f" . sp-forward-sexp)
         ("M-b" . sp-backward-sexp))

  :config
  ;; Enable smartparens everywhere
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  ;; Close a backtick with another backtick in clojure-mode
  (sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  :custom
  (sp-base-key-bindings 'paredit)
  :hook
  ;; (after-init . smartparens-global-mode)
  ((clojure-mode
    emacs-lisp-mode
    lisp-mode) . smartparens-strict-mode)
  (smartparens-mode  . sp-use-paredit-bindings)
  (smartparens-mode  . (lambda () (local-set-key (kbd "M-(") 'zz/sp-enclose-next-sexp)))
)


(use-package rainbow-delimiters
  :ensure
  :hook
  ((prog-mode cider-repl-mode clojure-mode) . rainbow-delimiters-mode))

(use-package clojure-mode-extra-font-locking
:ensure
:defer t)

;;;; cider
;; Clojure Interactive Development Environment that Rocks
(use-package cider
  :defer
  :init
  (setq cider-popup-stacktraces nil)
  (setq cider-repl-popup-stacktraces nil)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-use-clojure-font-lock t))

;; cider to work with org-mode
(use-package flycheck-clojure
  :ensure
  :defer t
  :init
  (add-hook 'emacs-startup-hook 'global-flycheck-mode)
  :config
  (use-package flycheck
    :config
    (flycheck-clojure-setup)))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (use-package flycheck
    :config
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

;;;; eval-sexp-fu
;; flash the region that is evaluated (visual feedback) in elisp
(use-package eval-sexp-fu
  :ensure t
  :bind
  (:map lisp-interaction-mode-map
        ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
        ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp)
        :map emacs-lisp-mode-map
        ("C-c C-c" . eval-sexp-fu-eval-sexp-inner-list)
        ("C-c C-e" . eval-sexp-fu-eval-sexp-inner-sexp))
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode))

;;;; outshine
;; outline with outshine outshines outline
(use-package outshine
  :blackout outline-minor-mode
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

;;;; dired
;; directory-browsing commands
(use-feature dired
  :after-call (dired-mode-hook)
  :init
  (defun my-find-name-dired (pattern)
    "Find files in `default-directory' using `rg' if available.
PREFIX forces the use of `find'."
    (interactive "sFind-name (filename wildcard): ")
    (if (and (not current-prefix-arg) (executable-find "rg"))
        (let ((find-program (concat "rg -g " (shell-quote-argument pattern) " --files"))
              (find-ls-option (cons "" "-dilsb")))
          (find-dired default-directory ""))
      (find-dired
       default-directory
       (concat find-name-arg " " (shell-quote-argument pattern)))))

  (setq dired-auto-revert-buffer t)
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  :bind (:map dired-mode-map ("`" . dired-toggle-read-only))
  :config
  ;; make rename use ido and not helm
  (put 'dired-do-rename 'ido 'find-file)
  ;; make copy use ido and not helm
  (put 'dired-do-copy 'ido 'find-file))

  ;; Rename files editing their names in dired buffers
  (use-package wdired
  :after-call (dired-mode-hook)
    :init
    ;; allow changing of file permissions
    (setq wdired-allow-to-change-permissions t))


(use-package diredfl :ensure
  :after-call (dired-mode-hook)
  :config (diredfl-global-mode))

;; dired+ adds some features to standard dired (like reusing buffers)
  (use-package dired+
    :after-call (dired-mode-hook)
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)

    :config
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package ediff
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-diff-options "-w")

  :config
  (add-hook! 'ediff-prepare-buffer-hook (when (derived-mode-p 'outline-mode) (outline-show-all)))
  (defvar ediff--saved-wconf nil)
  (defun ediff|save-wconf ()
    (setq ediff--saved-wconf (current-window-configuration)))
  (defun ediff|restore-wconf ()
    (set-window-configuration ediff--saved-wconf))
  (add-hook 'ediff-before-setup-hook #'ediff|save-wconf)
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) #'ediff|restore-wconf 'append))

(use-package magit :ensure
  :when (executable-find "git")
  :custom
  (magit-completing-read-function #'ivy-completing-read)
  (magit-diff-refine-hunk 'all)
  :config
    (defhydra hydra-magit (:color blue)
  "
  ^
  ^Magit^             ^Do^
  ^─────^─────────────^──^────────
  _q_ quit            _b_ blame
  ^^                  _c_ clone
  ^^                  _i_ init
  ^^                  _s_ status
  ^^                  ^^
  "
  ("q" nil)
  ("b" magit-blame-addition)
  ("c" magit-clone)
  ("i" magit-init)
  ("s" magit-status))
)

(use-package git-gutter
  :defer
  :after magit
  :init (global-git-gutter-mode +1))

(use-feature apropos
  :custom
  (apropos-do-all t))

(use-package ispell
  :when (executable-find "hunspell")
  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell t))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t))

(use-feature time
  :custom
  (display-time-24hr-format t)
  (display-time-load-average-threshold 1.5))

(use-feature tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist '(("." . "/tmp/tramp-backup-files/")))
  (tramp-auto-save-directory   (concat gas-cache-dir "tramp-auto-save/"))
  (tramp-persistency-file-name (concat gas-cache-dir "tramp-persistency.el"))
  (tramp-default-proxies-alist '(((regexp-quote (system-name)) nil nil)
                                 (nil "\\`root\\'" "/ssh:%h:"))))

(use-package treemacs :ensure
  :init
  (setq! treemacs-persist-file (concat gas-cache-dir "treemacs-persist")))

(bind-key "M-g" 'goto-line)

(bind-key "M-`" 'other-frame)

(bind-key "M-/" 'hippie-expand)

(defun zz/goto-match-paren (arg)
  "Go to the matching paren/bracket, otherwise (or if ARG is not nil) insert %.
  vi style of % jumping to matching brace."
  (interactive "p")
  (if (not (memq last-command '(set-mark
                                cua-set-mark
                                zz/goto-match-paren
                                down-list
                                up-list
                                end-of-defun
                                beginning-of-defun
                                backward-sexp
                                forward-sexp
                                backward-up-list
                                forward-paragraph
                                backward-paragraph
                                end-of-buffer
                                beginning-of-buffer
                                backward-word
                                forward-word
                                mwheel-scroll
                                backward-word
                                forward-word
                                mouse-start-secondary
                                mouse-yank-secondary
                                mouse-secondary-save-then-kill
                                move-end-of-line
                                move-beginning-of-line
                                backward-char
                                forward-char
                                scroll-up
                                scroll-down
                                scroll-left
                                scroll-right
                                mouse-set-point
                                next-buffer
                                previous-buffer
                                previous-line
                                next-line
                                back-to-indentation
                                )))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
          (t (self-insert-command (or arg 1))))))

(bind-key "%" 'zz/goto-match-paren)

(use-package which-key :ensure
  :defer 1
;  :blackout which-key-mode
  :after-call pre-command-hook
;  :bind (
;         ("<C-return>"     . gas/open-line-below)
;         ("<C-M-return>"   . gas/open-line-above)
;         ("<backtab>"      . indent-for-tab-command)
;         ("<C-tab>"        . indent-region)
;         ("M-o"            . other-window)
;         ("M-O"            . (λ! (other-window -1)))
;         ("C-x C-q"        . gas/exit-emacs)
;    )
  :config
  (setq! which-key-sort-order #'which-key-prefix-then-key-order
         which-key-sort-uppercase-first nil
         which-key-add-column-padding 1
         which-key-max-display-columns nil
         which-key-min-display-lines 6
         which-key-side-window-slot -10)
  (which-key-mode))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'emacs-startup-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'atea)
