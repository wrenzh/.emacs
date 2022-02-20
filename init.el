;;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-
;; Author: Wren Zhang (wren.zh@gmail.com)

;; Avoid garbage collection during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold #x40000000
      file-name-handler-alist nil
      site-run-file nil)
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (setq inhibit-compacting-font-caches t
		    file-name-handler-alist file-name-handler-alist-original)))

;; Load package.el and initialize use-package
(require 'package)
(setq package-native-compile t)
(setq package-quickstart t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil
	use-package-expand-minimally t))

;; Find platform-independent config file location
(defvar config-directory (file-name-directory user-init-file))

;; Avoid custom.el to auto-generate in init.el
(setq custom-file (concat config-directory "custom.el"))
(load custom-file 'noerror)

;; Basic graphical and usage configurations
;; Note some graphical settings are in early-init.el
(use-package emacs
  :custom
  (user-full-name "Wren Zhang")
  (frame-title-format '("Emacs"))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (redisplay-dont-pause t)
  (load-prefer-newer t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (backup-directory-alist (list (cons "." (concat config-directory "backup/"))))
  (backup-by-copying t)
  (confirm-kill-processes nil)
  (find-file-visit-truename t)
  (inhibit-startup-screen t)
  (fill-column 120)
  :config
  (blink-cursor-mode 0)
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package mouse
  :custom
  (scroll-margin 1)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (mouse-wheel-progressive-speed nil))

(use-package python
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  :hook (python-mode . electric-pair-mode))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

;; Spell checker with hunspell and flyspell
(use-package flyspell
  :defer 0.2
  :custom
  (ispell-program-name "hunspell")
  (ispell-personal-directory (concat config-directory "dictionary/"))
  :hook (text-mode . flyspell-mode))

(use-package elec-pair
  :hook (emacs-lisp-mode . electric-pair-mode))

;; Cleanup whitespaces when saving
(use-package whitespace
  :hook (before-save . whitespace-cleanup))

;; Display line numbers in prog-mode
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4)
  (setq display-line-numbers-type 'relative))

(use-package recentf
  :defer 1
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode t)
  (setq recentf-auto-cleanup 60)
  (run-at-time nil 120
	       #'(lambda () (let ((save-silently t)) (recentf-save-list))))
  (add-to-list 'recentf-exclude (concat config-directory "elpa/.*"))
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude ".*.synctex.gz")
  (add-to-list 'recentf-exclude "\\*.*\\*")
  (add-to-list 'recentf-exclude ".*~"))

(use-package xclip
  :if (not window-system)
  :config (xclip-mode t))

(use-package xt-mouse
  :if (not window-system)
  :config
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package gcmh
  :diminish gcmh-mode
  :config (gcmh-mode t))

(use-package diminish)

(use-package ivy
  :defer 0.2
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-use-selectable-prompt t)
  :bind ("C-s" . 'counsel-grep-or-swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :config (counsel-mode t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-redo)
  (setq evil-respect-visual-line-mode t)
  (setq evil-normal-state-modes
	(append evil-emacs-state-modes
		evil-normal-state-modes
		evil-motion-state-modes))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  :hook (after-init . evil-mode))

(use-package evil-surround
  :after evil
  :hook (prog-mode . evil-surround-mode))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :hook (after-init . evil-collection-init))

(use-package general
  :init
  (setq general-override-states '(insert
				  emacs
				  hybrid
				  normal
				  visual
				  motion
				  operator
				  replace))
  :config
  (general-evil-setup)
  (general-define-key
   :states '(normal visual motion)
   :prefix "SPC"
   :keymaps 'override
   "0" 'delete-window
   "1" 'delete-other-windows
   "b" 'ivy-switch-buffer
   "C" 'kill-buffer-and-window
   "c" 'kill-this-buffer
   "d" 'counsel-dired
   "e" 'counsel-find-file
   "f" 'counsel-rg
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "m" 'magit-status
   "q" 'save-buffers-kill-terminal
   "Q" 'save-buffers-kill-emacs
   "u" 'package-list-packages
   "w" 'save-buffer
   "x" 'counsel-M-x))

(use-package company
  :diminish company-mode
  :commands company-mode
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 2
	company-backends '(company-capf company-files company-dabbrev-code))
  :hook
  (emacs-lisp-mode . company-mode)
  (python-mode . company-mode))

(use-package olivetti
  :diminish olivetti-mode
  :custom (olivetti-body-width 80))

(use-package latex
  :ensure auctex
  :commands LaTeX-mode
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-save-query nil
	TeX-source-correlate-mode t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t)
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTex-mode . company-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . save-place-mode)
  (LaTeX-mode . turn-on-reftex))

(use-package magit
  :commands magit-status)

(use-package spice-mode
  :commands spice-mode
  :hook (spice-mode . display-line-numbers-mode)
  :mode ("\\.cir\\'" . spice-mode))

(use-package eglot
  :commands eglot
  :hook
  (python-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

(use-package eldoc-box
  :after eglot
  :diminish eldoc-box-hover
  :hook (eglot-managed-mode . (lambda () (eldoc-box-hover-at-point-mode t))))

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)))
  (setq org-src-tab-acts-natively t
	org-src-preserve-indentation t
	org-edit-src-content-indentation 0)
  :hook (org-mode . olivetti-mode))

(use-package feebleline
  :ensure t
  :config (feebleline-mode 1))
