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
  :config
  (setq user-full-name "Wren Zhang"
	frame-title-format '("Emacs")
	ring-bell-function 'ignore
	frame-resize-pixelwise t
	redisplay-dont-pause t
	load-prefer-newer t
	auto-save-default nil
	create-lockfiles nil
	backup-directory-alist (list (cons "." (concat config-directory "backup/")))
	backup-by-copying t
	confirm-kill-processes nil
	find-file-visit-truename t
	inhibit-startup-screen t
	fill-column 120)
  (blink-cursor-mode 0)
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package mouse
  :config
  (setq scroll-margin 1
	scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position t
	mouse-wheel-progressive-speed nil))

(use-package python
  :config
  (setq python-shell-interpreter "python3"))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

;; Spell checker with hunspell and flyspell
(use-package flyspell
  :delight
  :commands flyspell-mode
  :config
  (setq ispell-program-name "hunspell"
	ispell-personal-dictionary (concat config-directory "dictionary"))
  :hook (text-mode . flyspell-mode))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

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
  :defer t
  :init
  (setq recentf-max-saved-items 25)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-auto-cleanup 120)
  :config
  (recentf-mode t)
  (run-at-time nil 120 'recentf-save-list)
  (add-to-list 'recentf-exclude (concat config-directory "elpa/.*"))
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude ".*.synctex.gz")
  (add-to-list 'recentf-exclude "\\*.*\\*"))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package eldoc
  :init
  (global-eldoc-mode 0))

(use-package delight)

(use-package gcmh
  :delight
  :config (gcmh-mode t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package vterm)

(use-package ivy
  :delight
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp))

(use-package counsel
  :delight
  :after ivy
  :config
  (counsel-mode t))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

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
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (evil-mode t))

(use-package evil-surround
  :after evil
  :hook (prog-mode . evil-surround-mode))

(use-package evil-collection
  :delight evil-collection-unimpaired-mode
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package general
  :after evil-collection
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
   "" nil
   "0" 'delete-window
   "1" 'delete-other-windows
   "b" 'counsel-switch-buffer
   "C" 'kill-buffer-and-window
   "c" 'kill-this-buffer
   "d" 'counsel-dired
   "e" 'counsel-find-file
   "f" 'counsel-rg
   "m" 'magit-status
   "q" 'save-buffers-kill-terminal
   "Q" 'save-buffers-kill-emacs
   "u" 'package-list-packages
   "v" 'vterm
   "w" 'save-buffer
   "x" 'counsel-M-x))

(use-package company
  :delight
  :commands company-mode
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 2
	company-require-match nil
	company-tooltip-flip-when-above t
	company-tooltip-align-annotations t
	company-frontends '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
			    company-preview-common-frontend)
	company-backends '(company-capf))
  :hook (prog-mode . company-mode))

(use-package company-quickhelp
  :commands company-quickhelp-mode
  :config
  (setq company-quickhelp-delay nil)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
  :hook (prog-mode . company-quickhelp-mode))

(use-package olivetti
  :delight
  :delight visual-line-mode
  :custom (olivetti-body-width 80))

(use-package latex
  :commands LaTeX-mode
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . save-place-mode))

(use-package magit
  :commands magit-status)

(use-package rust-mode
  :commands rust-mode
  :config
  (setq rust-format-on-save t))

(use-package eglot
  :commands eglot
  :hook
  (rust-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (c-mode . eglot-ensure))

(use-package tree-sitter
  :commands tree-sitter-hl-mode
  :hook
  (rust-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode)
  (c-mode . tree-sitter-hl-mode))

(use-package tree-sitter-lang
  :defer t)

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
			       (python . t)))
  (setq org-image-actual-width nil)
  :hook
  (org-mode . olivetti-mode)
  (org-mode . save-place-mode))

(use-package moody
  :config
  (setq x-underline-at-descent-line t
	moody-mode-line-height 24)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))
