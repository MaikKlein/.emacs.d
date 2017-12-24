(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (setq use-package-always-ensure t))

(set-default-font "Source Code Pro 12")
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(blink-cursor-mode 0)
(save-place-mode 1) ;; Remember cursor position
(setq inhibit-startup-screen t) ;; No default welcome screen
(setq dotfile-path "~/.emacs.d/init.el")
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0) ;; Compiles the config files on start up

(use-package which-key
  :ensure t
    :init
    (which-key-mode)
    :config
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.2))

(use-package magit)

(use-package helm
  :ensure t
  :config
  (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit))

(use-package solarized-theme
  :ensure t
  :init
  (load-theme 'solarized-light t))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-define-key
      :states '(normal motion emacs)
      :prefix "SPC"
      "SPC" '(helm-M-x :which-key "Meta")
      "p"  '(:ignore t :which-key "Project")
      "pf" '(helm-projectile-find-file :which-key "Find file")
      "pp" '(helm-projectile-switch-project :which-key "Projectile")
      "f"  '(:ignore t :which-key "Files")
      "ff" '(helm-find-files :which-key "Find file")
      "fe"  '(:ignore t :which-key "Dotfiles")
      "fed" '(open-config :which-key "init.el")
      "fer" '(reload-config :which-key "init.el")
      "g"  '(:ignore t :which-key "Git")
      "gs" '(magit-status :which-key "git status")
      ";" '(evil-commentary :which-key "Comment")
      "l"  '(:ignore t :which-key "Workspace")
      "lc" '(perspeen-create-ws :which-key "Create workspace")
      "h"  '(:ignore t :which-key "Help")
      "hd"  '(:ignore t :which-key "Describe")
      "hdf"  '(describe-function :which-key "Describe function")
   )
)

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :after projectile)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

;; (use-package powerline-evil
;;   :config
;;   (powerline-evil-vim-theme))

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (evil-terminal-cursor-changer-activate)
  :config
  ())

(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab nil)
  :config
  (perspeen-mode))
(use-package helm-perspeen
  :ensure t
  :after perspeen)

(use-package evil-init
  :load-path "config/")

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package racer
  :ensure t
  :after 'rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'company-mode)
  (general-define-key :states '(normal)
                      :prefix "g"
                      "d" 'racer-find-definition))

(use-package company
  :ensure t
  :config
  (define-key company-active-map (kbd "C-SPC") 'company-complete))

(defun open-config ()
  (interactive)
  (find-file dotfile-path))

(defun reload-config ()
  (interactive)
  (load-file dotfile-path))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (safeplace which-key use-package solarized-theme racer powerline-evil magit helm-projectile general evil-terminal-cursor-changer evil-commentary counsel-projectile company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
