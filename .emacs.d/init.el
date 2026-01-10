(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Download packages unless they're already installed
(unless (package-installed-p 'poet-theme)
  (package-install 'poet-theme))
(unless (package-installed-p 'forth-mode)
  (package-install 'forth-mode))
(unless (package-installed-p 'sly)
  (package-install 'sly))
(unless (package-installed-p 'magit)
  (package-install 'magit))
(unless (package-installed-p 'doxymacs)
  (package-install 'doxymacs))
(unless (package-installed-p 'company)
  (package-install 'company))
(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

(setq history-length 2000)
(load-theme 'poet-dark t)
(electric-pair-mode 1)
(global-display-line-numbers-mode 1)
(setq make-backup-files nil)
(setq-default fill-column 80)
(setq whitespace-line-column 80)
(setq-default tab-width 4)
(setq-default truncate-lines t)

;; Peformance tweaks
(setq gc-cons-threshold 50000000)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

;; Some functions for quality of life, so to say
(defun kotorifan/check-gnus ()
  (interactive)
  (setq auth-file "~/.authinfo")
  (unless (file-exists-p 'auth-file)
	(setq username (read-string "Please enter the username for gnus"))
	(setq password (read-string "Please enter the password for gnus"))
	(write-region (concat ("machine news.eternal-september.org login"
						   username
						   " password "
						   password
						   "port 119"))
				  nil 'auth-file))
  (message "You may try opening gnus now"))

(defun kotorifan/doxygen ()
  "Generate doxygen documentation"
  (interactive)
  (let ((default-directory)
		(or (locate-dominating-file default-directory "Doxyfile")
			(default-directory)))
	(compile "doxygen Doxyfile")))

(defun kotorifan/check-empty ()
  "Check email for empty subject line"
  (interactive)
  (beginning-of-buffer)
  (if (search-forward "Subject: \n" nil t)
	  (error "The subject line is empty")))

(defun kotorifan/dl-file (&optional url download-dir download-name)
  "Download file over HTTP."
  (interactive)
  (let* ((url (or url (read-string "Enter download URL: ")))
         (dir (or download-dir "~/Downloads/"))
         (name (or download-name (file-name-nondirectory url)))
         (dest (expand-file-name name dir)))
    (url-copy-file url dest t)
    (if (file-exists-p dest)
        (message "The file was downloaded to %s" dest)
      (message "Download failed"))))

(defun kotorifan/check-server ()
  "Check if Emacs is running as a daemon"
  (if (daemonp)
	  (message "Emacs runs as a server")
	(message "Emacs doesn't run as a server")))

(setq c-default-style "linux"	  
  c-basic-offset 4)

(indent-tabs-mode 1)
(scroll-bar-mode  0)
(tool-bar-mode 0)
(global-display-line-numbers-mode t)
(setq global-display-line-numbers-mode 'relative)

;; Hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'mail-send-hook 'kotorifan/check-empty)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight medium :height 120 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a60b04e5c0fef30209f9576f04651938472b57cb1dae0375d80a53a78f515f69" "afde6368be6868e8e3dd53fad1ac51223d5484f9e6836496e7987802c9a9663d" default))
 '(global-display-line-numbers-mode 'relative)
 '(package-selected-packages
   '(poet-theme company sly magit forth-mode doxymacs almost-mono-themes))
 '(tool-bar-mode nil))
