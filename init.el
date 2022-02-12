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
  :defer 1
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
  (setq recentf-auto-cleaup 60)
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

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package diminish)

;; On macOS system the path is not inherited from shell
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer 1
  :config (exec-path-from-shell-initialize))

(use-package ivy
  :defer 0.5
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
	ivy-use-selectable-prompt t)
  :bind ("C-s" . 'swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :config (counsel-mode t))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-respect-visual-line-mode t)
  (evil-set-leader '(normal motion) (kbd "SPC"))
  (evil-define-key '(insert visual) 'global
    (kbd "jk") 'evil-normal-state)
  (evil-define-key '(normal visual) 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-define-key '(normal motion) 'global
    (kbd "<leader>0") 'delete-window
    (kbd "<leader>1") 'delete-other-windows
    (kbd "<leader>b") 'counsel-switch-buffer
    (kbd "<leader>d") 'dired
    (kbd "<leader>e") 'counsel-find-file
    (kbd "<leader>f") 'counsel-rg
    (kbd "<leader>C") 'kill-buffer-and-window
    (kbd "<leader>c") 'kill-this-buffer
    (kbd "<leader>w") 'save-buffer
    (kbd "<leader>h") 'evil-window-left
    (kbd "<leader>j") 'evil-window-down
    (kbd "<leader>k") 'evil-window-up
    (kbd "<leader>l") 'evil-window-right
    (kbd "<leader>u") 'list-packages
    (kbd "<leader>Q") 'save-buffers-kill-terminal)
  (evil-define-key 'normal 'emacs-lisp-mode-map
    (kbd "<leader>x") 'eval-last-sexp))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config (evil-collection-init 'magit))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))

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
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	bibte
	x-dialect 'biblatex
	TeX-source-correlate-start-server t)
  (evil-define-key 'normal 'latex-mode (kbd "<leader>w") 'TeX-command-run-all)
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTex-mode . company-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . save-place-mode)
  (LaTeX-mode . turn-on-reftex))

(use-package magit
  :defer 1
  :config
  (evil-define-key 'normal 'global (kbd "<leader>m") 'magit-status))

(use-package spice-mode
  :commands spice-mode
  :hook (spice-mode . display-line-numbers-mode)
  :mode ("\\.cir\\'" . spice-mode))

(use-package pyvenv
  :config
  (evil-define-key 'normal 'python-mode
    (kbd "<leader>a") 'pyvenv-activate
    (kbd "<leader>r") 'pyvenv-restart-python
    (kbd "<leader>x") 'python-shell-send-buffer))

(use-package eglot
  :commands eglot
  :hook
  (python-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

(use-package eldoc-box
  :after eglot
  :diminish eldoc-box-hover
  :hook (eglot--managed-mode . (lambda () (eldoc-box-hover-at-point-mode t))))

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)))
  (setq org-src-tab-acts-natively t
	org-src-preserve-indentation t
	org-edit-src-content-indentation 0)
  :hook (org-mode . olivetti-mode))

(use-package neotree
  :commands neotree-toggle
  :config
  (setq neo-window-width 30)
  (evil-leader/set-key "t" 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "o")
    'neotree-open-file-in-system-application))

(use-package gcmh
  :diminish gcmh-mode
  :config (gcmh-mode t))
