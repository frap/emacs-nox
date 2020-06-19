;;; core-nav.el --- Navigation fetaures of emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (amx-save-file (concat enfer-cache-dir "amx-items")))

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
  (defun enfer-company-dabbrev-buffer-check (buffer)
    (with-current-buffer buffer (derived-mode-p 'pdf-view-mode
                                                'doc-view-mode)))
  (setq! company-dabbrev-ignore-buffers #'enfer-company-dabbrev-buffer-check))

(use-package company-box
 :defer
 :after company
 :hook (company-mode . company-box-mode))

(provide 'core-nav)
;;; core-nav.el ends here
