;;;; init.el --- Emacs init file
;; Author: Wren Zhang

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.02
      file-name-handler-alist nil
      site-run-file nil)
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (setq inhibit-compacting-font-caches t
		    gc-cons-threshold 120000000
		    file-name-handler-alist file-name-handler-alist-original)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; File and edit settings
(use-package emacs
  :custom
  (user-full-name "Wren Zhang")
  (frame-title-format '("%b"))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (load-prefer-newer t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (backup-directory-alist `(("." . "~/.config/emacs/backups/")))
  (backup-by-copying t)
  (confirm-kill-processes nil)
  (find-file-visit-truename t)
  (inhibit-startup-screen t)
  :config
  (set-face-attribute 'default nil
		      :family "Sometype Mono"
		      :height (if (eq window-system 'ns) 120 100))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)
  (when (eq system-type 'gnu/linux) (setq dired-listing-switches "-alvFh --group-directories-first"))
  (add-to-list 'default-frame-alist (cons 'width 160))
  (add-to-list 'default-frame-alist (cons 'height 60)))

(use-package mouse
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed nil))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-no-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode t))

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  :hook
  (python-mode . electric-pair-mode))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode t))

(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "aspell")
  (ispell-personal-dictionary "~/.config/aspell/dictionary")
  :hook
  (text-mode . flyspell-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (elisp-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-type 'relative))

(use-package recentf
  :ensure nil
  :config
  (run-at-time nil 60
	       '(lambda () (let ((inhibit-message t)) (recentf-save-list))))
  (add-to-list 'recentf-exclude (format "%s/\\.config\\/emacs/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude ".*.synctex.gz")
  (add-to-list 'recentf-exclude "\\*.*\\*")
  :hook
  (after-init . (lambda ()
		  (setq recentf-auto-cleanup 'never)
		  (recentf-mode t)
		  (setq recentf-auto-cleanup 60))))

(use-package gruvbox-theme
  :hook
  (after-init . (lambda () (load-theme 'gruvbox-light-soft t))))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diminish)

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
				(t . ivy--regex-plus)))
  :hook (after-init . ivy-mode)
  :bind
  ("C-s" . 'swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :hook
  (ivy-mode . counsel-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :diminish ivy-posframe-mode
  :config
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
	'((left-fringe . 10) (right-fringe . 10)))
  (setq ivy-posframe-min-width 120)
  (setq ivy-posframe-height-alist '((t . 16)))
  (ivy-posframe-mode t))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-define-key 'normal 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line)
  (evil-define-key 'visual 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line))

(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-mode t)
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "b" 'ivy-switch-buffer
    "d" 'dired
    "e" 'find-file
    "q" 'kill-buffer-and-window
    "c" 'kill-this-buffer
    "w" 'save-buffer
    "h" 'evil-window-left
    "j" 'evil-window-bottom
    "k" 'evil-window-top
    "l" 'evil-window-right
    "o" 'other-window)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "x" 'eval-last-sexp))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode t))

(use-package key-chord
  :config
  (key-chord-define-global
   "jk" (lambda () (interactive)
	  (call-interactively (key-binding (kbd "<escape>")))))
  (key-chord-mode t))

(use-package pdf-tools
  :custom
  (pdf-view-use-scaling t))

(use-package olivetti
  :diminish olivetti-mode
  :custom
  (olivetti--visual-line-mode t)
  (olivetti-body-width 80))

(use-package latex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq TeX-save-query nil)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  (evil-leader/set-key-for-mode 'latex-mode "w" 'TeX-command-run-all)
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . pdf-loader-install)
  (LaTeX-mode . LaTeX-math-mode))

(use-package company
  :diminish company-mode
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t))

(use-package magit
  :ensure t
  :config
  (evil-leader/set-key "m" 'magit-status))

(use-package evil-magit)

(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography
	(concat (if (file-directory-p "~/Drive") "~/Drive" "~/OneDrive")
		"/Documents/Bibliography/library.bib"))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (evil-define-key 'insert LaTeX-mode-map (kbd "M-i") 'ivy-bibtex))

(use-package spice-mode
  :hook
  (spice-mode . display-line-numbers-mode)
  :mode
  ("\\.cir\\'" . spice-mode))

(use-package pyvenv
  :config
  (evil-leader/set-key-for-mode 'python-mode "a" 'pyvenv-activate)
  (evil-leader/set-key-for-mode 'python-mode "r" 'pyvenv-restart-python)
  (evil-leader/set-key-for-mode 'python-mode "x" 'python-shell-send-buffer)
  (evil-leader/set-key-for-mode 'python-mode "f" 'flymake-show-diagnostics-buffer))
(put 'dired-find-alternate-file 'disabled nil)

(use-package eglot)
