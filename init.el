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

(defun desktop-create-and-clear (name)
  "Create a new session, identified by a name.
The session is created in a subdirectory of `desktop+-base-dir'.
It can afterwards be reloaded using `desktop+-load'.

As a special case, if NAME is left blank, the session is
automatically named after the current working directory."
  (interactive "MDesktop name: ")
  (desktop-kill)
  (setq desktop-dirname (desktop+--dirname name))
  (make-directory desktop-dirname 'parents)
  (desktop-save desktop-dirname)
  (desktop+--set-frame-title)
  (desktop-save-mode 1)
  (desktop-clear)
  (dotimes (i 9)
    (eyebrowse--delete-window-config i)))

;; (defun find-structs ()
;;   (interactive)
;;   (counsel-rg "(type|enum|struct|trait)[\\t]+([a-zA-Z0-9_]+)" nil "-g '*.rs'" nil))

(defun find-structs ()
  (interactive
    (ivy-read "Find structs: "
              (counsel-rg "(type|enum|struct|trait)[\\t]+([a-zA-Z0-9_]+)" nil "-g '*.rs'" nil)
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'find-structs)))
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
(setq global-eldoc-mode nil)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(global-auto-revert-mode t)
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
(setq redisplay-dont-pause t)

(fset 'yes-or-no-p 'y-or-n-p)
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0) ;; Compiles the config files on start up
;; (use-package perspective
;;   :ensure t
;;   :config
;;   (persp-mode))
;; (use-package persp-projectile
;;   :ensure t)
;; (use-package persp-mode
;;   :ensure t
;;   :config
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
      ;; 
;; (use-package wconf
;;   :ensure t
;;   :config
;;   (add-hook 'desktop-after-read-hook      ;so we have all buffers again
;;           (lambda ()
;;             (wconf-load)
;;             (wconf-switch-to-config 0)
;;             (add-hook 'kill-emacs-hook
;;                       (lambda ()
;;                         (wconf-store-all)
;;                         (wconf-save))))
;;           'append))
;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (workgroups-mode 1))

(use-package rg
  :ensure t)
(use-package counsel-etags
  :ensure t
  :config
  (add-to-list 'counsel-etags-ignore-filenames "TAGS"))
(use-package avy
  :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package desktop+
  :ensure t)
(use-package eyebrowse
  :ensure t
  :config
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))
  (eyebrowse-mode t))

;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (setq wg-prefix-key (kbd "C-c z"))
;;   ;; Change workgroups session file
;;   (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;;   ;; What to do on Emacs exit / workgroups-mode exit?
;;   ;; (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
;;   ;; (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil
;;   (workgroups-mode 1))
;; Sorts queries from ivy
(use-package smex
  :ensure t)
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
   "," '(cargo-process-fmt :which-key "Format")
   "e" '(:ignore t :which-key "Error")
   "en" '(flycheck-next-error :which-key "Next error")
   "p" '(:ignore t :which-key "Project")
   "pf" '(counsel-projectile-find-file :which-key "Find file")
   "pp" '(counsel-projectile-switch-project :which-key "Projectile")
   "pg" '(counsel-rg :which-key "Grep")
   "f" '(:ignore t :which-key "Files")
   "ff" '(counsel-find-file :which-key "Find file")
   "fs" '(find-structs :which-key "Find file")
   "fl" '(counsel-locate :which-key "Locate")
   "fe" '(:ignore :which-key "Dotfiles")
   "fed" '(open-config :which-key "init.el") "fer" '(reload-config :which-key "init.el")
   "g" '(:ignore t :which-key "Git")
   "gs" '(magit-status :which-key "git status")
   ";" '(evil-commentary :which-key "Comment")
   "o" '(:ignore t :which-key "Workspace")
   "o1" '(eyebrowse-switch-to-window-config-1 :which-key "Window 1")
   "o2" '(eyebrowse-switch-to-window-config-2 :which-key "Window 2")
   "o3" '(eyebrowse-switch-to-window-config-3 :which-key "Window 3")
   "o4" '(eyebrowse-switch-to-window-config-4 :which-key "Window 4")
   "o5" '(eyebrowse-switch-to-window-config-5 :which-key "Window 5")
   "o6" '(eyebrowse-switch-to-window-config-6 :which-key "Window 6")
   "o7" '(eyebrowse-switch-to-window-config-7 :which-key "Window 7")
   "o7" '(eyebrowse-switch-to-window-config-7 :which-key "Window 7")
   "o8" '(eyebrowse-switch-to-window-config-8 :which-key "Window 8")
   "o9" '(eyebrowse-switch-to-window-config-9 :which-key "Window 9")
   "ok" '(eyebrowse-close-window-config :which-key "Close Window")
   "l" '(:ignore t :which-key "Workspace")
   "lc" '(desktop-create-and-clear :which-key "Create desktop")
   "ll" '(desktop+-load :which-key "Load desktop")
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
   "sr" '(ivy-resume :which-key "Resume last search")
   "sd" '(avy-goto-char-2 :which-key "Goto char 2")
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



(use-package cargo
  :ensure t)
(use-package
  flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-navigation-minimum-level 'error)
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
  ;;(add-hook 'racer-mode-hook #'eldoc-mode) 
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
 '(electric-pair-mode t)
 '(global-eldoc-mode nil)
 '(package-selected-packages
   (quote
    (counsel-etags rg cargo avy rainbow-delimiters desktop+ wconf persp-projectile eyebrowse counsel ivy workgroups2 winum which-key use-package toml-mode spaceline-all-the-icons solarized-theme racer powerline-evil persp-mode markdown-mode helm-swoop helm-projectile helm-ls-git general flycheck-rust evil-terminal-cursor-changer evil-surround evil-magit evil-commentary diminish csharp-mode company airline-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
