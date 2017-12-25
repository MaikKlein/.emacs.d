(use-package evil
    :ensure t
    :init
    (evil-mode)
    :config
    (evil-declare-abort-repeat 'save-buffer-always)
    (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit))

(use-package evil-commentary
  :ensure t)

(use-package evil-magit
  :ensure t)

(provide 'evil-init)
