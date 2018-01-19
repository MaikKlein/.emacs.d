(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos)) 
                    (not (gnutls-available-p)))) 
       (url (concat (if no-ssl "http" "https")
                    "://melpa.org/packages/"))) 
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" .
                                   "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package) 
  (setq use-package-always-ensure t))

;;(set-default-font "Source Code Pro 12")
(setq default-frame-alist '((font . "Source Code Pro 12")))
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(save-place-mode 1)             ;; Remember cursor position
(setq inhibit-startup-screen t) ;; No default welcome screen
(setq dotfile-path "~/.emacs.d/init.el")
(setq evil-want-C-u-scroll t)
;; Smooth scrolling
(setq scroll-step 1) 
(setq scroll-preserve-screen-position 1)
(setq scroll-conservatively 10000)
(setq gc-cons-threshold 8000000)
(fringe-mode 5)
(define-fringe-bitmap 'empty-fringe
  (vector #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000
          #b0000))

;; No popups anymore
(setq use-dialog-box nil)
(setq-default indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0) ;; Compiles the config files on start up

;; (use-package persp-mode
;;   :ensure t
;;   :config
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
      ;; 
;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (workgroups-mode 1))

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))
(use-package counsel
  :ensure t
  :config
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key swiper-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (counsel-mode 1))

(use-package counsel-projectile
  :ensure t)
(use-package toml-mode
  :ensure t)
;; (use-package helm-swoop
;;   :ensure t
;;   :config
;;   (setq helm-swoop-speed-or-color t))
;; (use-package helm-ls-git
;;   :ensure t)
(use-package evil-magit
  :ensure t)
(use-package csharp-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-enable-math t)
  (setq markdown-command "multimarkdown | smartypants"))

(use-package 
  diminish 
  :ensure t 
  :config
  ;; Remove useless mode line infos
  (diminish 'undo-tree-mode) 
  (diminish 'auto-revert-mode) 
  (diminish 'abbrev-mode))

;; (use-package persp-mode
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (persp-mode 1))

(use-package 
  winum 
  :ensure t 
  :config (winum-mode))

(use-package 
  which-key 
  :ensure t 
  :init (which-key-mode) 
  :config (setq which-key-sort-order 'which-key-key-order-alpha
                which-key-side-window-max-width 0.33
                which-key-idle-delay 0.2) 
  :diminish "")

(use-package magit
  :ensure t)

;; (use-package 
;;   helm 
;;   :ensure t 
;;   :config
;;   ;; Forces helm to stick to the bottom
;;   (setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
;;   (add-to-list 'display-buffer-alist `(,(rx bos "*helm" (*
;;                                                          not-newline)
;;                                             "*" eos) 
;;                                        (display-buffer-in-side-window) 
;;                                        (inhibit-same-window . t) 
;;                                        (window-height . 0.4)))
;;   (helm-mode 1))


(use-package 
  solarized-theme 
  :ensure t 
  :config (load-theme 'solarized-light t)
  ;; Gets rid of the misaligned line
  (setq x-underline-at-descent-line t))

(use-package 
  general 
  :ensure t 
  :config
  ;;(setq general-default-keymaps 'evil-normal-state-map)
  (general-evil-setup t) 
  (general-define-key :states '(insert) 
                      "C-SPC" 'company-complete) 
  (general-define-key :states '(insert normal) 
                      "C-s" 'save-buffer-always)

  ;; (general-define-key
  ;;   :states '(normal motion emacs)
  ;;   "TAB" 'perspeen-tab-next)
  ;; bind gj and gk
  (general-define-key
   :states '(normal motion emacs) 
   :prefix "SPC" 
   "SPC" '(counsel-M-x :which-key "Meta")
   "p" '(:ignore t :which-key "Project")
   "pf" '(counsel-projectile-find-file :which-key "Find file")
   "pp" '(counsel-projectile-switch-project :which-key "Projectile")
   "pg" '(counsel-rg :which-key "Grep")
   "f" '(:ignore t :which-key "Files")
   "ff" '(counsel-find-file :which-key "Find file")
   "fe" '(:ignore :which-key "Dotfiles")
   "fed" '(open-config :which-key "init.el") "fer" '(reload-config :which-key "init.el")
   "g" '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   ";" '(evil-commentary :which-key "Comment")
   "l" '(:ignore t :which-key "Workspace")
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
   "b" '(:ignore t :which-key "Buffer")
   "bb" '(ivy-switch-buffer :which-key "Buffer list")
   "s" '(:ignore t :which-key "Search")
   "ss" '(swiper :which-key "Swoop")
   "h" '(:ignore t :which-key "Help")
   "hd" '(:ignore t :which-key "Describe")
   "hdk" '(describe-key :which-key "Describe key")
   "hdf" '(describe-function :which-key "Describe function")
   "hdv" '(describe-variable :which-key "Describe variable")
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9))

(use-package 
  projectile 
  :ensure t 
  :diminish "" 
  :config (setq projectile-enable-caching t) 
  (setq projectile-require-project-root nil) 
  (projectile-mode +1))

;; (use-package 
;;   helm-projectile 
;;   :ensure t 
;;   :after projectile)

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))
;;
;; (use-package powerline-evil
;;   :config
;;   (powerline-evil-vim-theme))

(use-package 
  evil-terminal-cursor-changer 
  :ensure t 
  :init (evil-terminal-cursor-changer-activate) 
  :config ())

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

(use-package 
  evil-init 
  :load-path "config/")


;; (define-fringe-bitmap 'flycheck-fringe-bitmap-ball (vector #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000
;;                                                            #b00000000))



(use-package
  flycheck
  :ensure t
  :config
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'empty-fringe
    :error-list-face 'flycheck-error-list-error
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'empty-fringe
    :error-list-face 'flycheck-error-list-warning
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'empty-fringe
    :error-list-face 'flycheck-error-list-info
    :fringe-face 'flycheck-fringe-info))
  ;; :config (flycheck-define-error-level 'warning 
  ;;           :fringe-bitmap 'flycheck-fringe-bitmap-ball) 
  ;; (flycheck-define-error-level 'error 
  ;;   :severity 100 
  ;;   :compilation-level 2 
  ;;   :overlay-category 'flycheck-error-overlay 
  ;;   :fringe-bitmap 'flycheck-fringe-bitmap-ball 
  ;;   :fringe-face 'flycheck-fringe-error 
  ;;   :error-list-face 'flycheck-error-list-error))
(use-package 
  flycheck-rust 
  :ensure t 
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(defun rust-init () 
  (interactive) 
  (general-define-key 
                      :prefix "g" 
                      :states '(normal) 
                      "d" 'racer-find-definition) 
  (rust-mode) 
  (racer-mode) 
  ;;(lsp-rust)
  (eldoc-mode) 
  (company-mode) 
  (flycheck-mode))
;; (use-package lsp-mode
;;   :ensure t)
;; (use-package lsp-rust
;;   :after lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))
(use-package 
  rust-mode 
  :ensure t 
  :config
  ;;(add-hook 'rust-mode-hook #'lsp-rust-enable)
  ;;(add-hook 'rust-mode-hook #'company-mode)

  (add-hook 'rust-mode-hook #'racer-mode) 
  (add-hook 'racer-mode-hook #'eldoc-mode) 
  (add-hook 'racer-mode-hook #'flycheck-mode)
  (add-hook 'racer-mode-hook #'company-mode) 
  (add-hook 'racer-mode-hook (lambda () 
                                (interactive)
                                (general-define-key :keymap 'rust-mode-map 
                                                    :prefix "g" 
                                                    :states '(normal) 
                                                    "d"
                                                    'racer-find-definition))) 
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package 
  racer 
  :ensure t 
  :after 'rust-mode)
;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends))

(use-package 
  company 
  :ensure t 
  :config
  ;; Disable auto completion
  (setq company-idle-delay nil)
  (company-mode))

(defun open-config () 
  (interactive) 
  (find-file dotfile-path))

(defun reload-config () 
  (interactive) 
  (load-file dotfile-path))

;; (use-package 
;;   spaceline 
;;   :ensure t
;;   ;;:init
;;   ;;(use-package spaceline-config)
;;   :config (require 'spaceline-config) 
;;   (spaceline-spacemacs-theme) 
;;   (setq spaceline-highlight-face-func
;;         'spaceline-highlight-face-evil-state) 
;;   (spaceline-compile))
(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package powerline-evil
  :ensure t)
  


;; (use-package 
;;   spaceline-all-the-icons 
;;   :ensure t)

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
    (counsel ivy workgroups2 winum which-key use-package toml-mode spaceline-all-the-icons solarized-theme racer powerline-evil persp-mode markdown-mode helm-swoop helm-projectile helm-ls-git general flycheck-rust evil-terminal-cursor-changer evil-surround evil-magit evil-commentary diminish csharp-mode company airline-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
