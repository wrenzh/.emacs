;; After emacs-27 early-init.el allows loading graphical settings
;; early in the startup stack and therefore speeds up

;; Visual settings
(set-face-attribute 'default nil
		    :family "Jetbrains Mono"
		    :height 110)
(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 60))
(scroll-bar-mode 0)
(tool-bar-mode 0)

(menu-bar-mode 0)
(fringe-mode 0)
