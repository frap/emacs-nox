;; early-init.el -*- lexical-binding: t -*-
;; Author: copied from weirdNox (Andrés Gasson)

(defconst emacs-start-time (current-time))

(defvar initial--file-name-handler-alist file-name-handler-alist)

(defun gas|reset-temporary-init-values ()
  "Resets garbage collection settings to reasonable defaults (if you don't do
this, you'll get stuttering and random freezes) and resets `file-name-handler-alist'."
  (setq file-name-handler-alist initial--file-name-handler-alist
        gc-cons-threshold 16777216
        gc-cons-percentage 0.15))

(unless noninteractive
  (unless after-init-time
    (setq package-enable-at-startup nil
          gc-cons-threshold 402653184
          gc-cons-percentage 0.6
          message-log-max 16384
          file-name-handler-alist nil
          auto-window-vscroll nil))
  (add-hook 'emacs-startup-hook #'gas|reset-temporary-init-values))

(setq load-prefer-newer noninteractive
      package-enable-at-startup nil)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
