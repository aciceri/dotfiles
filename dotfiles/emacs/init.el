(require 'use-package)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(setq display-time-format "%I:%M:%S")
(display-time-mode 1)


(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(set-face-attribute 'default t :font "Source Code Pro-12")

(package-initialize)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
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
	  ([?\s-c] . exwm-reset)

	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-g] . exwm-floating-toggle-floating)

	  ([?\s-t] . helm-exwm)

	  ([?\s-q] . kill-current-buffer)
	  
	  ([?\s-b] . (lambda () (interactive) ;; starts qutebrowser or get qutebrowser buffers with helm
		       (if (seq-filter (lambda (buffer) (string-match "qutebrowser" (buffer-name buffer))) (buffer-list))
			   (helm-exwm (function (lambda ()
						  (string-match "qutebrowser" (or exwm-class-name "")))))
			 (start-process "" nil "qutebrowser"))))


     
	  ([?\s-d] . helm-run-external-command)))

 
  (defun exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

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
  (evil-mode 1))

(use-package evil-collection
  :after (evil company-mode)
  :config
  (evil-collection-init))

(use-package org-evil)

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

(use-package company
  :config (global-company-mode))

;;(use-package helm-company)

(use-package nix-mode
  :mode "\\.nix\\'")

;;(use-package helm-spotify-plus)
