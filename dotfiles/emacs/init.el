(require 'use-package)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(set-face-attribute 'default t :font "Source Code Pro-12")

(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook 
	    (lambda () (load-theme 'cyberpunk t))))

(use-package exwm
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

(use-package telephone-line
  :config
  (progn
    (setq telephone-line-lhs
	  '((evil   . (telephone-line-evil-tag-segment))
	    (accent . (telephone-line-vc-segment
		       telephone-line-erc-modified-channels-segment
		       telephone-line-process-segment))
	    (nil    . (telephone-line-minor-mode-segment
		       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
	  '((nil    . (telephone-line-misc-info-segment))
	    (accent . (telephone-line-major-mode-segment))
	    (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode t)))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org-evil)

(use-package helm
  
  :init
  (progn
    (require 'helm-config)
    (setq helm-autoresize-max-height 0)
    (setq helm-autoresize-min-height 20)
    (helm-autoresize-mode 1)
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
	  helm-buffer-max-length nil)

    (helm-mode 1))

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

(use-package helm-exwm)

(use-package company               
  :init (global-company-mode))

;;(use-package helm-company)

(use-package nix-mode
  :mode "\\.nix\\'")
