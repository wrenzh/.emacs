;; After emacs-27 early-init.el allows loading graphical settings
;; early in the startup stack and therefore speeds up

;; Visual settings
(set-face-attribute 'default nil
		    :family "Fantasque Sans Mono"
		    :height (if (eq window-system 'ns) 140 120))
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)
(tool-bar-mode 0)
(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 60))
