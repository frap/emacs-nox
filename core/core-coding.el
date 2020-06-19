;;; core/core-coding.el -*- lexical-binding: t; -*-

;;;; prog-mode
;; Generic major mode for programming
(use-feature prog-mode
  :config
  (defun my-prog-mode-hook ()
    (setq show-trailing-whitespace 1)
    (prettify-symbols-mode 1))
  :hook (prog-mode . my-prog-mode-hook))


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

(use-package rust-mode
  :ensure
  :defer
  :mode ("\\.rs" . rust-mode)
)

;; a great lisp coding hook
(defun enfer-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq enfer-lisp-coding-hook 'enfer-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun enfer-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq enfer-interactive-lisp-coding-hook 'enfer-interactive-lisp-coding-defaults)

(use-feature lisp-mode
  :mode ("\\.lte" . lisp-mode)
  )

;(with-eval-after-load 'clojure-mode
;  (defun enfer-clojure-mode-defaults ()
;    (subword-mode +1)
;    (run-hooks 'enfer-lisp-coding-hook))

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
  (add-hook! :append :local clojure-mode '(yas-minor-mode subword-mode global-prettify-symbols-mode enfer-lisp-coding-hook))
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
(use-package ob-clojurescript
  :ensure
  :defer
  :config
  (setq org-babel-clojure-backend 'cider))

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

(use-package typescript-mode
:ensure t)

;; from the Tide README
(defun setup-tide-mode ()
  "Set up Tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
)

(use-package web-mode :ensure
  :mode (("\\.\\(go\\)?html?\\'" . web-mode)))

(use-package sgml-mode
  :mode ("\\.html\\'" . html-mode)
  :config (add-hook 'html-mode-hook 'turn-off-auto-fill))

(use-package erc
  :commands erc
  :init
  (setq
   erc-hide-list '("JOIN" "PART" "QUIT")
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-timestamp-format "[%H:%M] "
   erc-timestamp-only-if-changed-flag nil
   erc-truncate-mode t)
  :config
  (add-hook
   'window-configuration-change-hook
   (lambda () (setq erc-fill-column (- (window-width) 2)))))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
       (font-lock-add-keywords 'org-mode
        '(("^ +\\([-*]\\) "
               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
 :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out)
         ("C-M-|" . indent-rigidly)
         :map org-mode-map
         ("M-n" . outline-next-visible-heading)
         ("M-p" . outline-previous-visible-)
         ("M-C-n" . org-end-of-item-list)
         ("M-C-p" . org-beginning-of-item-list)
         ("C-s-f" . forward-sentence)
         ("C-s-b" . backward-sentence)
        )
  :config
  (setq! org-modules '(org-habit org-id org-protocol org-timer org-bullets))
  (use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(setq! org-directory "~/org/")
(defconst enfer-org-agenda-file (concat org-directory "atea.org"))
(defconst enfer-org-journal-file (concat org-directory "journal.org"))
(setq! org-default-notes-file (concat org-directory "inbox.org")
       org-agenda-files (list enfer-org-agenda-file))

(setq! org-startup-indented t
       org-startup-with-inline-images t
       org-startup-with-latex-preview t
       org-pretty-entities t
       org-image-actual-width '(700)
       org-fontify-quote-and-verse-blocks t)

(add-hook! org-mode #'org-hide-block-all)

(setq!  org-tags-column -92
        org-ellipsis " ↴ "
        org-catch-invisible-edits 'smart
        org-return-follows-link t
        org-list-allow-alphabetical t
        org-loop-over-headlines-in-active-region t
        org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

(setq!
 org-todo-keywords '(
     (sequence "TODO(t)" "PROCHAINE(n)" "|" "FINI(f)")
     (sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "SUSPENDUE(h@/!)" "ATTENTE(w@/!)" "|" "ANNULÉE(c@/!)"))
 org-treat-S-cursor-todo-selection-as-state-change nil
 org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
 org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 7:00")))

(setq! org-refile-use-outline-path 'file
       org-outline-path-complete-in-steps nil
       org-refile-allow-creating-parent-nodes 'confirm
       org-refile-targets `((nil . (:maxlevel . 9))
                            (org-agenda-files . (:maxlevel . 9))))

(add-hook! 'org-after-refile-insert-hook
  (org-up-heading-safe)
  (org-update-statistics-cookies nil))

(use-package hydra
  :config
  ;; Define the templates
  (setq org-structure-template-alist
        '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
          ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
          ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
          ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
          ("n" "#+begin_note\n?\n#+end_note" "<note>\n?\n/note>")
          ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
          ("l" "#+begin_export latex\n?\n#+end_export" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
          ("h" "#+begin_export html\n?\n#+end_exrt" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+html: " "<literal style=\"html\">?</literal>")
          ("a" "#+begin_export ascii\n?\n#+end_export")
          ("A" "#+ascii: ")
          ("i" "#+index: ?" "#+index: ?")
          ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure
--------------------------------------------------------------------------------------
_C_: center        _s_: src         _L_: LATEX:
_q_: quote         _e_: emacs lisp  _i_: index:
_E_: example       _p_: python      _I_: INCLUDE:
_v_: verse         _P_: perl        _H_: HTML:
_a_: ascii         _u_: Plantuml    _A_: ASCII:
_l_: latex         _d_: ditaa
_h_: html          _S_: shell
_n_: note          _c_: clojure
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("C" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("n" (hot-expand "<n"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("P" (hot-expand "<s" "perl"))
    ("j" (hot-expand "<s" "java"))
    ("c" (hot-expand "<s" "clojure"))
    ("S" (hot-expand "<s" "sh"))
    ("d" (hot-expand "<s" "ditaa :file CHANGE.png :cache yes"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1)))))

(setq! org-log-done 'time
       org-log-reschedule 'time
       org-log-into-drawer t)

(setq! org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (octave . t)
   (clojure . t)
   (python . t)
   (latex . t)
   (shell . t)
   (calc . t)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(defun +babel-get-src-info ()
  "Return (LANG . SESSION)."
  (let* ((info (org-babel-get-src-block-info t))
         (params (nth 2 info)))
    (cons (car info)
          (cdr (assq :session params)))))

(defun +babel/kill-session ()
  "Kill session for current code block."
  (interactive)
  (org-babel-when-in-src-block
   (let ((config (current-window-configuration)))
     (org-babel-switch-to-session)
     (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
     (kill-buffer)
     (set-window-configuration config))))

(defun +babel/restart-session-to-point (&optional arg)
  "Restart session up to the src-block in the current point.
Goes to beginning of buffer and executes each code block with
`org-babel-execute-src-block' that has the same language and
session as the current block. ARG has same meaning as in
`org-babel-execute-src-block'."
  (interactive "P")
  (org-babel-when-in-src-block
   (let ((search-bound (point-marker))
         (info (+babel-get-src-info))
         break)
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (and (not break) (re-search-forward org-babel-src-block-regexp nil t))
        (goto-char (match-beginning 0))
        (if (> (point) search-bound)
            (setq break t)
          (when (equal info (+babel-get-src-info)) (org-babel-execute-src-block arg)))
        (forward-line))))))

(defun +babel/remove-session-results ()
  "Remove results from every code block of the selected session, in buffer."
  (interactive)
  (org-babel-when-in-src-block
   (let ((info (+babel-get-src-info)))
     (org-with-wide-buffer
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (when (equal info (+babel-get-src-info))
          (org-babel-remove-result)))))))

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@bureau" . ?o)
                            ("@maison" . ?h)
                            ("@ferme"  . ?f)
                            (:newline)
                            ("ATTENTE"  . ?w)
                            ("SUSPENDUE" . ?H)
                            ("ANNULÉE"    . ?c)
                            ("RÉUNION"   . ?m)
                            ("TÉLÉPHONE" . ?p))))

);; use-package



(use-feature org-agenda
  :config
  (defun +agenda|check-sync-conflicts ()
    (when (directory-files org-directory nil "sync-conflict")
      (message-box "CONSEIL: Il y a des conflits de synchronisation!")))
  (add-hook 'org-agenda-finalize-hook #'+agenda|check-sync-conflicts)
  (setq!
   org-agenda-custom-commands
   '(("n" "Ordre de jour"
      ((tags-todo "@important"
                  ((org-agenda-overriding-header "Tâches hautement importante")))
       (tags-todo "@urgent"
                  ((org-agenda-overriding-header "Tâches à faible priorité")))
       (tags-todo "TODO/!@urgent"
                  ((org-agenda-overriding-header "Tâches isolées")))
       (tags-todo "ATTENTE"
                  ((org-agenda-overriding-header "Tâches en attente")))
       )))
   org-agenda-prefix-format '((agenda . "  %?-12t% s"))
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-tags-todo-honor-ignore-options t
   org-agenda-todo-ignore-scheduled 'all
   org-agenda-todo-ignore-deadlines 'far
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-clockreport-parameter-plist `(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 100)
   org-agenda-columns-add-appointments-to-effort-sum t
   org-agenda-dim-blocked-tasks nil
   org-agenda-todo-list-sublevels nil
   org-agenda-block-separator ""
   org-agenda-time-grid '((daily today require-timed) nil "......" "----------------")
   )

  )

(use-feature org-capture
  :init
  (defun enfer-org-capture-frame ()
    (modify-frame-parameters nil '((name . "Org Capture")
                                   (org-capture-frame . t)
                                   (width . 110) (height . 40)))
    (org-capture))

  :config
  (setq!
   org-capture-templates
      '(("t" "T" entry (file "")
         "* PROCHAINE %i%?" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "~/org/gtd/inbox.org")
         "* PROCHAINE Respond to %:from on %:subject :@bureau:\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("c" "Calendrier" entry (file "")
         "* %?\n%^t")
        ("n" "Remarque" entry (file "")
         "* %?" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree enfer-org-journal-file))
        ("m" "Meeting" entry (file "~/org/gtd/calendars/atea-cal.org")
         "* RÉUNION with %? :RÉUNION:@bureau:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file+headline "~/org/gtd/atea.org" "Interruptions")
         "* TÉLÉPHONE %? :TÉLÉPHONE:@bureau:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit🙈" entry (file "~/org/gtd/atea.org")
         "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: PROCHAINE\n:END:\n")
        ("w" "Web bookmark" entry (file "")
         "* [[%:link][%^{Title|%:description}]]\n%?" :clock-in t :clock-resume t))
      ))

  ;; NOTE(gas): Handle capture frame
  (advice-add
   'org-switch-to-buffer-other-window :after
   (lambda (&rest _) (when (frame-parameter nil 'org-capture-frame) (delete-other-windows))))
  (advice-add
   'org-capture :around
   (lambda (capture-function &rest args)
     (condition-case nil (apply capture-function args)
       (error (when (frame-parameter nil 'org-capture-frame)
                (delete-frame))))))
  (add-hook
   'org-capture-after-finalize-hook
   (lambda (&rest _)
     (when (and (frame-parameter nil 'org-capture-frame) (not org-capture-is-refiling))
       (org-save-all-org-buffers)
       (delete-frame))))
  (advice-add
   'org-capture-refile :after
   (lambda (&rest _)
     (when (frame-parameter nil 'org-capture-frame)
       (org-save-all-org-buffers)
       (delete-frame))))

(use-feature org-clock
  :config
  (setq! org-clock-in-resume t
         org-clock-out-remove-zero-time-clocks t
         org-clock-report-include-clocking-task t
         org-clock-persist t
         org-clock-persist-file (concat enfer-etc-dir "org-clock-save.el")
         org-clock-history-length 25)
  (org-clock-persistence-insinuate))

(use-feature org-habit
  :config
  (setq! org-habit-graph-column 75
         org-habit-preceding-days 30
         org-habit-following-days 1
         org-habit-today-glyph ?@))

(use-feature org-src
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  :config
  (add-to-list 'org-src-lang-modes '("html" . web)))

;;;; outshine
;; outline with outshine outshines outline
(use-package outshine
  :blackout outline-minor-mode
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))



(provide 'core-coding)
;;; core-coding.el ends here
