;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;;; Author: frap (Andrés Gasson)


(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Doom is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; foe emacs < 27 load early-init
;;(unless (boundp 'early-init-file) (load (concat (file-name-directory load-file-name) "early-init") nil t))

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
;;(load (concat user-emacs-directroy "config"))
;; Load the heart of Enfer Emacs
(load (concat user-emacs-directory "core/atea")
      nil 'nomessage)


;; let her rip!
;;(enfer-initialise)
