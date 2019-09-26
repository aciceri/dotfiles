(setq inhibit-startup-message t)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
(set-face-attribute 'default t :font "Source Code Pro-14")

(load-theme 'spacemacs-dark t) 

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(display-line-numbers-mode)

(provide 'appearance)
