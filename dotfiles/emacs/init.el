(require 'use-package)

(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(setq display-time-format "%H:%M")
(display-time-mode 1)
(setq mouse-autoselect-window 't)

(setq inhibit-startup-screen t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(set-face-attribute 'default t :font "Source Code Pro-12")

(package-initialize)

(server-start)

(setq async-shell-command-buffer 'new-buffer)

(use-package doom-themes
  :after treemacs
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package exwm
  :if window-system
  :config
  (progn
    (require 'seq)
    
  (setq exwm-input-global-keys
	`(
	  ([?\s-c] . exwm-reset) ; works?

	  ([?\s-w] . exwm-workspace-switch)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))

	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-g] . exwm-floating-toggle-floating)

	  ([?\s-t] . helm-exwm)

	  ([?\s-q] . kill-current-buffer)

	  ([?\s-m] . (lambda () (interactive)
		       (async-shell-command "spotify")
		       (async-shell-command "spotify-adkiller")))

	  ([?\s-b] . (lambda () (interactive)
		       (start-process "" nil "qutebrowser")))
	  
	  ([?\s-d] . helm-run-external-command)))

 
  (defun exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

  (setq exwm-workspace-show-all-buffers t)
  
  (setq window-divider-default-bottom-width 2
	window-divider-default-right-width 2)
  (window-divider-mode)

  (exwm-input-set-key (kbd "s-h") #'windmove-left)
  (exwm-input-set-key (kbd "s-j") #'windmove-down)
  (exwm-input-set-key (kbd "s-k") #'windmove-up)
  (exwm-input-set-key (kbd "s-l") #'windmove-right)
  
  (exwm-enable)))

(use-package windsize
  :after exwm
  :config (progn
	    (windsize-default-keybindings)
	    (exwm-input-set-key (kbd "s-H") #'windsize-left)
	    (exwm-input-set-key (kbd "s-J") #'windsize-down)
	    (exwm-input-set-key (kbd "s-K") #'windsize-up)
	    (exwm-input-set-key (kbd "s-L") #'windsize-right)
	    ))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (progn
    (evil-mode 1) ; globally enable evil-mode except for the following modes
    (mapcar (lambda (mode) (evil-set-initial-state mode 'emacs))
	   '(vterm-mode
	     eshell-mode
	     dired-mode
	     ))))

(use-package evil-collection
  :after (evil company-mode vterm)
  :config
    (evil-collection-init))

(use-package org-evil)

(use-package vterm)

(use-package helm
  
  :init
  (progn
    (require 'helm-config)
    (setq helm-autoresize-max-height 0)
    (setq helm-autoresize-min-height 20)
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (when (executable-find "ack")
      (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
	    helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

    (setq helm-semantic-fuzzy-match t
	  helm-imenu-fuzzy-match t
	  helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match t
	  helm-split-window-in-side-p t
	  helm-buffer-max-length nil)

    (helm-mode 1)
    (helm-autoresize-mode 1))

  :bind
  (("C-c h" . helm-command-prefix)
   :map helm-command-map
   ("b" . helm-buffers-list)
   ("f" . helm-find-files)
   ("m" . helm-mini)
   ("o" . helm-imenu))
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files))
  )

(use-package helm-exwm
  :after (exwm helm)
  :config (setq helm-exwm-buffer-max-length nil)
)

(use-package projectile
  :config
  (progn
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)))

(use-package helm-projectile
  :after projectile
  :config
  (progn
    (helm-projectile-on)))

(use-package treemacs)

(use-package treemacs-evil
  :after treemacs)

(use-package company
  :config (global-company-mode))

;;(use-package helm-company)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package company-nixos-options
  :after company
  :config
  (progn
    (add-to-list 'company-backends 'company-nixos-options)))

(use-package helm-nixos-options)

(use-package nnreddit)
