;;;; init.el --- Emacs init file
;; Author: Wren Zhang

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil)
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (setq inhibit-compacting-font-caches t
		    gc-cons-threshold 20000000
		    gc-cons-percentage 0.1
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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; File and edit settings
(use-package emacs
  :custom
  (user-full-name "Wren Zhang")
  (frame-title-format '("Emacs"))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  (load-prefer-newer t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (backup-directory-alist `((".")))
  (backup-by-copying t)
  (confirm-kill-processes nil)
  (inhibit-startup-screen t)
  :config
  (set-face-attribute 'default nil
		      :family "Sometype Mono"
		      :height (if (eq system-type 'darwin) 120 100))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode 0)
  (scroll-bar-mode 0)
  (when (eq system-type 'gnu/linux)
    (setq x-gtk-use-system-tooltips nil
	  dired-listing-switches "-alhG --group-directories-first")))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 2)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode t))

(use-package python
  :ensure nil
  :custom
  (python-indent-offet 4)
  (python-shell-interpreter "python3"))

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
  :hook
  (text-mode . flyspell-mode))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

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
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  :hook
  (after-init . (lambda ()
		  (setq recentf-auto-cleanup 'never)
		  (recentf-mode t)
		  (setq recentf-auto-cleanup 60))))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package mood-line
  :config
  (mood-line-mode t))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
				(swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy)))
  :hook
  (after-init . ivy-mode )
  :bind
  ("C-s" . 'swiper))

(use-package counsel
  :after ivy
  :hook
  (ivy-mode . counsel-mode))

(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  (ivy-posframe-mode t))

(use-package company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t))

(use-package company-quickhelp
  :hook
  (after-init . company-quickhelp-mode))


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
  (global-evil-leader-mode)
  (evil-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "b" 'switch-to-buffer
    "d" 'dired
    "e" 'find-file
    "l" 'ibuffer
    "o" 'other-window
    "q" 'kill-this-buffer
    "w" 'save-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "x" 'eval-last-sexp)
  (evil-leader/set-key-for-mode 'python-mode "x" 'python-shell-send-buffer))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-mc
  :config
  (global-evil-mc-mode t))

(use-package undo-tree
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
  (evil-leader/set-key-for-mode 'latex-mode "i" 'ivy-bibtex)
  :hook
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . pdf-loader-install)
  (LaTeX-mode . LaTeX-math-mode))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 80)
  (visual-fill-column-center-text t)
  :hook
  (visual-line-mode . visual-fill-column-mode)
  (visual-fill-column-mode . (lambda () (setq visual-fill-column-center-text t))))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :commands lsp)

(use-package poetry
  :hook
  (python-mode . poetry-tracking-mode))

(use-package magit
  :ensure t
  :config
  (evil-leader/set-key "m" 'magit-status))

(use-package evil-magit)

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography
	(concat (if (file-directory-p "~/Drive")
		    "~/Drive" "~/OneDrive")
		"/Documents/Bibliography/library.bib"))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package vterm
  :config
  (evil-leader/set-key "v" 'vterm))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package neotree
  :custom
  (neo-theme 'nerd)
  :config
  (evil-leader/set-key "t" 'neotree-toggle))

(use-package all-the-icons)

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
	centaur-tabs-set-icons t
	centaur-tabs-set-close-button nil
	centaur-tabs-set-bar 'under
	x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  (evil-leader/set-key "j" 'centaur-tabs-forward)
  (evil-leader/set-key "k" 'centaur-tabs-backward)
  :hook
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode))
