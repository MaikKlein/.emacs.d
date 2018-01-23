(use-package evil
    :ensure t
    :config
    (evil-declare-abort-repeat 'save-buffer-always)
    (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
    (evil-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t)

(use-package evil-magit
  :ensure t)
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(provide 'evil-init)
