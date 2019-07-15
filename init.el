;; Improve emacs speed
(setq inhibit-compacting-font-caches nil)
(setq gc-cons-threshold 80000000)

;; Custom and package initialization
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
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
(add-hook 'after-init-hook (lambda () (recentf-mode t)))
(add-hook 'after-init-hook (lambda () (electric-pair-mode t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'hl-line-mode)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))
(global-unset-key "\C-z")

;; Display settings
(add-hook 'after-init-hook
	  (lambda () (set-face-attribute 'default nil :family "Sometype Mono" :height 100)))
(add-hook 'after-init-hook (lambda () (blink-cursor-mode 0)))
(add-hook 'after-init-hook (lambda () (menu-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (tool-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (scroll-bar-mode 0)))
(add-hook 'after-init-hook (lambda () (show-paren-mode 1)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq frame-title-format "%b")
(setq ring-bell-function 'ignore)
(when (eq system-type 'gnu/linux) (setq x-gtk-use-system-tooltips nil))
(when (window-system) (set-frame-size (selected-frame) 100 40))
(setq dired-listing-switches "-alhG --group-directories-first")

(use-package diminish
  :ensure t)

(use-package monokai-theme
  :ensure t
  :hook
  (after-init . (lambda ()(load-theme 'monokai t))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  :hook
  (after-init . (lambda () (ivy-mode t)))
  :bind
  ("C-s" . 'swiper))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook
  (after-init . counsel-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :hook
  (after-init . (lambda () (global-company-mode t))))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-really-aspell t)
  :hook
  (text-mode . flyspell-mode))

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (setq flycheck-python-flake8-executable "python3")
  (setq flycheck-python-pycompile-executable "python3")
  (setq flycheck-python-pylint-executable "python3")
  :hook
  (after-init . global-flycheck-mode))

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "b" 'switch-to-buffer
    "d" 'dired
    "e" 'find-file
    "g" 'keyboard-escape-quit
    "l" 'list-buffers
    "o" 'other-window
    "j" 'next-buffer
    "k" 'previous-buffer
    "q" 'kill-this-buffer
    "r" (lambda () (interactive) (revert-buffer nil t))
    "t" 'neotree-toggle
    "w" 'save-buffer)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "x" 'eval-last-sexp)
  (evil-leader/set-key-for-mode 'python-mode "r" 'pyvenv-restart-python)
  (evil-leader/set-key-for-mode 'latex-mode "w" 'TeX-command-run-all))

(use-package evil
  :ensure t
  :defer t
  :config
  (global-evil-leader-mode)
  (evil-mode t)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(use-package evil-mc
  :ensure t
  :diminish evil-mc-mode
  :hook
  (after-init . global-evil-mc-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-visualizer-diff t)
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
	  (if (eq evil-state 'emacs) (evil-exit-emacs-state) (evil-emacs-state))))
  :hook
  (after-init . (lambda () (key-chord-mode t))))

(use-package latex
  :ensure auctex
  :diminish visual-line-mode
  :diminish auto-fill-function
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
  :hook
  (LaTeX-mode . turn-on-auto-fill)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . pdf-loader-install)
  (LaTeX-mode . LaTeX-math-mode)
  (TeX-after-compilation-finished-functions . (revert-buffer t)))

(use-package company-auctex
  :ensure t
  :hook
  (after-init . company-auctex-init))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package elpy
  :ensure t
  :hook
  (after-init . elpy-enable))

(server-start)
