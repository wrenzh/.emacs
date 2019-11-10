;; Improve emacs speed
(setq inhibit-compacting-font-caches nil)
(setq gc-cons-threshold #x1000000)

;; Custom and package initialization
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; File and edit settings
(setq auto-save-default nil)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq create-lockfiles nil)
(add-hook 'after-init-hook (lambda () (electric-pair-mode t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'hl-line-mode)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))
(global-unset-key "\C-z")
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

;; Display settings
(add-hook 'after-init-hook
	  (lambda () (set-face-attribute
		      'default nil :family "Sometype Mono" :height 100)))
(add-hook 'after-init-hook (lambda () (blink-cursor-mode 0)))
(add-hook 'after-init-hook (lambda () (menu-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (tool-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (scroll-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (show-paren-mode 1)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq frame-title-format "%b")
(setq ring-bell-function 'ignore)
(when (eq system-type 'gnu/linux)
  (setq x-gtk-use-system-tooltips nil
	dired-listing-switches "-alhG --group-directories-first"))
(when (version< "26" emacs-version)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))
(setq display-line-numbers-type 'relative)

(use-package all-the-icons
  :ensure t)

(use-package diminish
  :ensure t)

(use-package recentf
  :ensure nil
  :config
  (recentf-mode t)
  (setq recentf-max-menu-items 25)
  (setq recentf-max-saved-items 25)
  (run-at-time nil (* 5 60) '(lambda ()
			       (let ((inhibit-message t))
				 (recentf-save-list)))))

(use-package monokai-theme
  :ensure t
  :hook
  (after-init . (lambda () (load-theme 'monokai t))))

(use-package mood-line
  :ensure t
  :config
  :hook
  (after-init . mood-line-mode))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((ivy-bibtex . ivy--regex-ignore-order)
	  (swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  :hook
  (after-init . (lambda () (ivy-mode t)))
  :bind
  ("C-s" . 'swiper))

(use-package counsel
  :ensure t
  :after ivy
  :hook
  (after-init . counsel-mode))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :hook
  (after-init . (lambda () (ivy-posframe-mode t))))

(use-package company
  :ensure t
  :hook
  (after-init . (lambda () (global-company-mode t))))

(use-package company-quickhelp
  :ensure t
  :hook
  (after-init . company-quickhelp-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-really-aspell t)
  :hook
  (text-mode . flyspell-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'icons))

(use-package evil
  :ensure t
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
  :ensure t
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
    "g" 'keyboard-escape-quit
    "l" 'ibuffer
    "m" 'magit-status
    "o" 'other-window
    "j" 'next-buffer
    "k" 'previous-buffer
    "q" 'kill-this-buffer
    "r" (lambda () (interactive) (revert-buffer nil t))
    "t" 'neotree-toggle
    "w" 'save-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "x" 'eval-last-sexp)
  (evil-leader/set-key-for-mode 'python-mode "x" 'python-shell-send-buffer)
  (evil-leader/set-key-for-mode 'latex-mode "w" 'TeX-command-run-all)
  (evil-leader/set-key-for-mode 'latex-mode "i" 'ivy-bibtex))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t))

(use-package evil-mc
  :ensure t
  :hook
  (after-init . global-evil-mc-mode))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	'(("." . "~/.emacs.d/undo/")))
  :hook
  (after-init . global-undo-tree-mode))

(use-package key-chord
  :ensure t
  :config
  (key-chord-define-global
   "jk" (lambda () (interactive)
	  (call-interactively (key-binding (kbd "<escape>")))))
  (key-chord-define-global
   "zz" (lambda () (interactive)
	  (if (eq evil-state 'emacs) (evil-exit-emacs-state)
	    (evil-emacs-state))))
  :hook
  (after-init . (lambda () (key-chord-mode t))))

(use-package pdf-tools
  :ensure t)

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
  (setq tex-directory "~/.tex")
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . pdf-loader-install)
  (LaTeX-mode . LaTeX-math-mode))

(use-package visual-fill-column
  :ensure t
  :config
  (setq visual-fill-column-width 80)
  (setq visual-fill-column-center-text t)
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package company-math
  :ensure t
  :config
  (add-to-list 'company-backends
	       'company-math-symbols-unicode))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package poetry
  :ensure t
  :hook
  (python-mode . poetry-tracking-mode))

(use-package ebib
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package ivy-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography
	"~/Drive/Documents/Bibliography/library.bib")
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))
