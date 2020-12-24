;;;; init.el --- Emacs init file
;; Author: Wren Zhang

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.3
      file-name-handler-alist nil
      site-run-file nil)
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (setq inhibit-compacting-font-caches t
		    file-name-handler-alist file-name-handler-alist-original)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

(use-package emacs
  :custom
  (user-full-name "Wren Zhang")
  (frame-title-format '("%b"))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t)
  (redisplay-dont-pause t)
  (load-prefer-newer t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (backup-directory-alist `(("." . "~/.config/emacs/backups/")))
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
  :init (setq show-paren-delay 0)
  :config (show-paren-mode t))

(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-personal-dictionary "~/.config/emacs/dictionary")
  :hook (text-mode . flyspell-mode))

(use-package elec-pair
  :ensure nil
  :hook (emacs-lisp-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

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
  (add-to-list 'recentf-exclude
	       (format "%s/\\.config\\/emacs/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude ".*.synctex.gz")
  (add-to-list 'recentf-exclude "\\*.*\\*")
  :hook
  (after-init . (lambda ()
		  (setq recentf-auto-cleanup 'never)
		  (recentf-mode t)
		  (setq recentf-auto-cleanup 60))))

(use-package autorevert
  :ensure nil
  :config (global-auto-revert-mode t))

(use-package gruvbox-theme
  :hook (after-init . (lambda () (load-theme 'gruvbox-light-medium t))))

(use-package diminish)

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 6)
  (gcmh-mode t))

(when (eq window-system 'ns)
  (use-package exec-path-from-shell
    :defer 1
    :config
    (exec-path-from-shell-initialize)))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
				(swiper . ivy--regex-plus)
				(t . ivy--regex-plus)))
  :hook (after-init . ivy-mode)
  :bind ("C-s" . 'swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode))

(use-package ivy-rich
  :after ivy-posframe
  :config
  (setq ivy-rich-display-transformers-list
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-switch-buffer-transformer (:width 40))
	    (ivy-rich-switch-buffer-size (:width 7))
	    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	    (ivy-rich-switch-buffer-project (:width 15 :face success))
	    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path
							      x (ivy-rich-minibuffer-width 0.3))))))
	   :predicate
	   (lambda (cand) (get-buffer cand)))
	  counsel-find-file
	  (:columns
	   ((ivy-read-file-transformer (:width 40))
	    (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
	  counsel-M-x
	  (:columns
	   ((counsel-M-x-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring (:width 40 :face font-lock-doc-face))))
	  counsel-describe-function
	  (:columns
	   ((counsel-describe-function-transformer (:width 40))
	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	  counsel-describe-variable
	  (:columns
	   ((counsel-describe-variable-transformer (:width 40))
	    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
	  counsel-recentf
	  (:columns
	   ((ivy-rich-candidate (:width 40))
	    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
	  package-install
	  (:columns
	   ((ivy-rich-candidate (:width 40))
	    (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
	    (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
	    (ivy-rich-package-install-summary (:face font-lock-doc-face))))))
  (ivy-rich-mode 0)
  (ivy-rich-mode t))

(use-package ivy-posframe
  :diminish ivy-posframe-mode
  :after ivy
  :config
  (setq ivy-posframe-height 10)
  (setq ivy-posframe-width 80)
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
	'((left-fringe . 10)
	  (right-fringe . 10)))
  (ivy-posframe-mode t))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (setq evil-respect-visual-line-mode t)
  (evil-define-key 'normal 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line)
  (evil-define-key 'visual 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line))

(use-package undo-fu)

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
    "C" 'kill-buffer-and-window
    "c" 'kill-this-buffer
    "w" 'save-buffer
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "o" 'other-window)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "x" 'eval-last-sexp))

(use-package evil-collection
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
  :defer 1
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :hook
  (emacs-lisp-mode . (lambda () (company-mode t))))

(use-package yasnippet
  :diminish yas-mode
  :defer 1
  :config
  (evil-define-key 'insert 'global
    (kbd "TAB") 'yas-expand)
  (yas-global-mode t))

(use-package yasnippet-snippets
  :defer 2)

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
  (setq TeX-view-program-list
	     '("Zathura" ("zathura "
		(mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
		" %o") "zathura"))
  (setq TeX-view-program-selection '((output-pdf "Zathura")))
  (setq bibtex-dialect 'biblatex)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  (evil-leader/set-key-for-mode 'latex-mode "w" 'TeX-command-run-all)
  :hook
  (LaTeX-mode . electric-pair-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . LaTeX-math-mode))

(use-package magit
  :defer 2
  :config
  (evil-leader/set-key "m" 'magit-status))

(use-package ivy-bibtex
  :after ivy
  :defer 1
  :config
  (setq bibtex-completion-bibliography
	"~/OneDrive/Documents/Bibliography/library.bib")
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
    "x" 'python-shell-send-buffer
    "f" 'flymake-show-diagnostics-buffer))

(use-package eglot
  :hook
  (python-mode . eglot-ensure))
(put 'dired-find-alternate-file 'disabled nil)

(use-package doom-modeline
  :config (setq doom-modeline-icon nil)
  :hook (after-init . doom-modeline-mode))

(use-package org
  :config
  (setq org-agenda-files (list "~/OneDrive/Documents/Notes/Schedule.org")))
