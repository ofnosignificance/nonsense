(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; The theme I use
(use-package poet-theme
  :config (load-theme 'poet-dark t))

;; Languages
(use-package forth-mode)
(use-package markdown-mode)
(use-package json-mode)
(use-package sly
  :init (setq inferior-lisp-program "clisp"))
(use-package cmake-mode
  :ensure t)

;; Development tools
(use-package magit)
(use-package company
  :config (global-company-mode t))
(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))
(use-package format-all
  :ensure t
  :hook (c++-mode . format-all-mode))

;; Other
(use-package multiple-cursors
  :bind ("C-S-c C-S-c" . mc/edit-lines))
(use-package jinx
  :hook (hook-mode . jinx-mode))

(defun kotorifan/check-gnus ()
"Generate a configuration for Gnus, if none exists"
  (interactive)
  (let ((auth-path (expand-file-name "~/.authinfo")))
    (unless (file-exists-p auth-path)
      (let ((username (read-string "Please enter the username for gnus: "))
            (password (read-string "Please enter the password for gnus: ")))
        (write-region
         (format "machine news.eternal-september.org login %s password %s port 119\n"
                 username password)
         nil auth-path)))
    (message "You may try opening gnus now")))

(defun kotorifan/doxygen ()
"Generate documentation with Doxygen"
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "Doxyfile")
                               default-directory)))
    (compile "doxygen Doxyfile")))

(defun kotorifan/check-empty ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Subject: *$" nil t)
        (error "The subject line is empty"))))

(defun kotorifan/dl-file (&optional url download-dir download-name)
  "Downloads a file to a given path, if no path is given, it goes to ~/Downloads"
  (interactive)
  (let* ((url (or url (read-string "Enter download URL: ")))
         (dir (or download-dir "~/Downloads/"))
         (name (or download-name (file-name-nondirectory url)))
         (dest (expand-file-name name dir)))
    (unless (file-exists-p dir) (make-directory dir t))
    (url-copy-file url dest t)
    (if (file-exists-p dest)
        (message "The file was downloaded to %s" dest)
      (message "Download failed"))))

(defun kotorifan/check-server ()
  "Checks whether Emacs runs as a daemon"
  (interactive)
  (if (daemonp)
      (message "Emacs runs as a server")
    (message "Emacs doesn't run as a server")))

(setq c-default-style "linux"
      c-basic-offset 4)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(electric-pair-mode 1)

(setq-default tab-width 4
              truncate-lines t
              fill-column 72
              indent-tabs-mode nil)

(setq show-paren-style 'parenthesis
      global-hl-line-sticky-flag t
      display-line-numbers-type 'relative
      electric-indent-mode nil
      make-backup-files nil
      history-length 2000
      whitespace-line-column 72
      inferior-lisp-program "sbcl")

(global-display-line-numbers-mode t)
(global-hl-line-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-send-hook 'kotorifan/check-empty)
(add-hook 'after-init-hook 'ruler-mode)
(setq gc-cons-threshold (* 50 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(format-all company-c-headers company-doxygen yasnippet-snippets yasnippet cmake-font-lock cmake-mode modern-cpp-font-lock sly poet-theme multiple-cursors markdown-mode magit json-mode jinx forth-mode doxymacs company))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "IBM Plex Mono" :foundry "IBM " :slant normal :weight medium :height 120 :width normal)))))
(put 'downcase-region 'disabled nil)
