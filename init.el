;;;; init.el --- Emacs init file
;; Author: Wren Zhang (wren.zh@gmail.com)

;; Avoid garbage collection during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.3
      file-name-handler-alist nil
      site-run-file nil)
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (setq inhibit-compacting-font-caches t
		    file-name-handler-alist file-name-handler-alist-original)))

;; Load package.el and initialize use-package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

;; Find platform-independent config file location
(defvar config-directory (file-name-directory user-init-file))

;; Avoid custom.el to auto-generate in init.el
(setq custom-file (concat config-directory "custom.el"))
(load custom-file 'noerror)

;; Basic graphical and usage configurations
;; Note some graphical settings are in early-init.el
(use-package emacs
  :ensure nil
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
  (defalias 'yes-or-no-p 'y-or-n-p)
  (when (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alvFh --group-directories-first")))

(use-package mouse
  :ensure nil
  :custom
  (scroll-margin 1)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (mouse-wheel-progressive-speed nil))

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  :hook (python-mode . electric-pair-mode))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

;; Spell checker with hunspell and flyspell
(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-personal-directory (concat config-directory "dictionary/"))
  :hook (text-mode . flyspell-mode))

(use-package elec-pair
  :ensure nil
  :hook (emacs-lisp-mode . electric-pair-mode))

;; Cleanup whitespaces when saving
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Display line numbers in prog-mode
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4)
  (setq display-line-numbers-type 'relative))

(use-package recentf
  :ensure nil
  :config
  (run-at-time nil 120
	       '(lambda () (let ((save-silently t)) (recentf-save-list))))
  (add-to-list 'recentf-exclude (concat config-directory "elpa/.*"))
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude ".*.synctex.gz")
  (add-to-list 'recentf-exclude "\\*.*\\*")
  (add-to-list 'recentf-exclude ".*~")
  :hook
  (after-init . (lambda ()
		  (setq recentf-auto-cleanup 'never)
		  (recentf-mode t)
		  (setq recentf-auto-cleanup 60))))

(use-package autorevert
  :ensure nil
  :config (global-auto-revert-mode t))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :hook (after-init . global-subword-mode))

(use-package gruvbox-theme
  :if window-system
  :hook (after-init . (lambda () (load-theme 'gruvbox-dark-medium t))))

(use-package diminish)

(use-package gcmh
  :diminish gcmh-mode
  :config (gcmh-mode t))

;; On macOS system the path is not inherited from shell
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer 1
  :config (exec-path-from-shell-initialize))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-use-selectable-prompt t
	ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
				(swiper . ivy--regex-plus)
				(t . ivy--regex-plus)))
  :hook (after-init . ivy-mode)
  :bind ("C-s" . 'swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode))

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (setq evil-respect-visual-line-mode t)
  (evil-define-key '(normal visual) 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-mode t)
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "5" 'make-frame-command
    "b" 'ivy-switch-buffer
    "d" 'dired
    "e" 'find-file
    "f" 'counsel-rg
    "C" 'kill-buffer-and-window
    "c" 'kill-this-buffer
    "w" 'save-buffer
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "o" 'other-window
    "u" 'list-packages
    "Q" 'kill-emacs)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "x" 'eval-last-sexp))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode t))

(use-package key-chord
  :config
  (key-chord-define-global
   "jk" (lambda () (interactive)
	  (call-interactively (key-binding (kbd "<escape>")))))
  (key-chord-mode t))

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 2
	company-backends '(company-capf company-yasnippet company-files company-dabbrev-code))
  :hook
  (emacs-lisp-mode . company-mode)
  (python-mode . company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package yasnippet-snippets
  :defer 0.5)

(use-package olivetti
  :diminish olivetti-mode
  :custom (olivetti-body-width 80))

(use-package latex
  :ensure auctex
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
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (evil-leader/set-key-for-mode 'latex-mode "w" 'TeX-command-run-all)
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTex-mode . company-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . save-place-mode))

(use-package magit
  :defer 0.5
  :config
  (evil-leader/set-key "m" 'magit-status))

(use-package ivy-bibtex
  :after ivy
  :config
  (setq bibtex-completion-bibliography
	"~/iCloud/Documents/Bibliography/library.bib")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (evil-define-key 'insert LaTeX-mode-map (kbd "M-i") 'ivy-bibtex))

(use-package spice-mode
  :hook (spice-mode . display-line-numbers-mode)
  :mode ("\\.cir\\'" . spice-mode))

(use-package pyvenv
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "a" 'pyvenv-activate
    "r" 'pyvenv-restart-python
    "x" 'python-shell-send-buffer))

(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (c++-mode . eglot-ensure))

(use-package eldoc-box
  :diminish eldoc-box-hover
  :hook (eglot--managed-mode . (lambda () (eldoc-box-hover-at-point-mode t))))

(use-package org
  :hook (org-mode . olivetti-mode))

(use-package neotree
  :config
  (setq neo-window-width 30)
  (evil-leader/set-key "t" 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "o")
    'neotree-open-file-in-system-application))
