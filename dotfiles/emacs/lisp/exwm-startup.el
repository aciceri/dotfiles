(defun exwm-startup ()
  (setq inhibit-startup-message t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)

  (add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
  (set-face-attribute 'default t :font "Source Code Pro-12")

  
  
  (setq exwm-workspace-number 10)
  
  (setq exwm-input-global-keys
	`(
	  ([?\s-c] . (lambda (command) (load-file "init.el")))

	  ([?\s-f] . exwm-layout-toggle-fullscreen)
	  ([?\s-g] . exwm-layout-toggle-floating)

	  ([?\s-w] . exwm-workspace-switch)
	  
	  ([?\s-d] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))

	  
	  ([?\s-b] . (lambda () (interactive) (start-process "" nil "qutebrowser")))))
  
  (defun exwm-rename-buffer-to-title ()
    (exwm-workspace-rename-buffer exwm-title))
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)
  
  (exwm-enable))

(provide 'exwm-startup)
