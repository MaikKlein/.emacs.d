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
(use-package persp-mode
  :ensure t
  :config
  (persp-mode 1))

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package which-key
  :ensure t
    :init
    (which-key-mode)
    :config
    (setq which-key-sort-order 'which-key-key-order-alpha
          which-key-side-window-max-width 0.33
          which-key-idle-delay 0.2)
    :diminish which-key-mode)

(use-package magit)

(use-package helm
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)
  ;; Gets rid of the misaligned line
  (setq x-underline-at-descent-line t))

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  ;;(setq general-default-keymaps 'evil-normal-state-map)

  (general-define-key
   :states '(insert)
   "C-SPC" 'company-complete)

  (general-define-key
   :states '(insert normal)
   "C-s" 'save-buffer-always)

  ;; (general-define-key
  ;;   :states '(normal motion emacs)
  ;;   "TAB" 'perspeen-tab-next)

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
   "lc" '(wg-create-workgroup :which-key "Create workspace")
   ;; "lc" '(wg-create-workgroup :which-key "Create workspace")
   ;; "lk" '(wg-kill-workgroup :which-key "Kill workspace")
   ;; "l1" '(wg-switch-to-workgroup-at-index-0 :which-key "Workspace 1")
   ;; "l2" '(wg-switch-to-workgroup-at-index-1 :which-key "Workspace 2")
   ;; "l3" '(wg-switch-to-workgroup-at-index-2 :which-key "Workspace 3")
   ;; "l4" '(wg-switch-to-workgroup-at-index-3 :which-key "Workspace 4")
   ;; "lc" '(perspeen-create-ws :which-key "Create workspace")
   ;; "l1" '((lambda () (interactive) (perspeen-goto-ws 1)) :which-key "Workspace 1")
   ;; "l2" '((lambda () (interactive) (perspeen-goto-ws 2)) :which-key "Workspace 2")
   ;; "l3" '(ws3 :which-key "Workspace 3")
   "h"  '(:ignore t :which-key "Help")
   "hd"  '(:ignore t :which-key "Describe")
   "hdk" '(describe-key :which-key "Describe key")
   "hdf" '(describe-function :which-key "Describe function")
   )
)

;; (defmacro goto-ws (idx)
;;   (lambda () (interactive) (perspeen-goto-ws idx)))

;; (defun ws2 ()
;;   (interactive)
;;   (perspeen-goto-ws 2))

;; (defun ws3 ()
;;   (interactive)
;;   (perspeen-goto-ws 3))

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :after projectile)

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))
;; 
;; (use-package powerline-evil
;;   :config
;;   (powerline-evil-vim-theme))

(use-package evil-terminal-cursor-changer
  :ensure t
  :init
  (evil-terminal-cursor-changer-activate)
  :config
  ())

;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (setq wg-mode-line-display-on t)  
;;   (workgroups-mode 1))
	    
;; (use-package perspeen
;;   :ensure t
;;   :init
;;   (setq perspeen-use-tab nil)
;;   :config
;;   (perspeen-mode))
;; (use-package helm-perspeen
;;   :ensure t
;;   :after perspeen)

(use-package evil-init
  :load-path "config/")

(defun rust ()
  (rust-mode)
  (racer-mode)
  (eldoc-mode)
  (company-mode)
  (flycheck-mode))

(define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))



(use-package flycheck
  :ensure t
  :config
  (flycheck-define-error-level 'warning
    :fringe-bitmap 'flycheck-fringe-bitmap-ball)
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error))
(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust)))

(use-package racer
  :ensure t
  :after 'rust-mode
  :config 
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(use-package company
  :ensure t
  :config
  ;; Disable auto completion
  (setq company-idle-delay nil))

(defun open-config ()
  (interactive)
  (find-file dotfile-path))

(defun reload-config ()
  (interactive)
  (load-file dotfile-path))

(use-package spaceline
  :ensure t
  ;;:init
  ;;(use-package spaceline-config)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-compile))


(use-package spaceline-all-the-icons
  :ensure t)

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer)
  (evil-normal-state))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (persp-mode winum flycheck-rust flycheck workgroups2 which-key use-package spaceline-all-the-icons solarized-theme racer powerline-evil helm-projectile helm-perspeen general evil-terminal-cursor-changer evil-magit evil-escape evil-commentary counsel-projectile company color-theme-sanityinc-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
