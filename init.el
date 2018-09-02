;; (package-initialize)

;; disable automatic description as this is both annoying and can easily
;; get intero stuck
(global-eldoc-mode -1)

(add-hook 'minibuffer-setup-hook
    (lambda () (setq truncate-lines nil)))

(setq resize-mini-windows t) ; grow and shrink as necessary
(setq max-mini-window-height 10) ; grow up to max of 10 lines

(setq minibuffer-scroll-window t)

;; will search for cabal in these directories
;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path "~/.local/bin")

;; load packages
(load "~/.emacs.d/my-loadpackages.el")

;; cycle through buffers with Ctrl-Tab
(global-set-key (kbd "s-'") 'other-window)


(global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)          ; move to lower window

(global-set-key (kbd "C-~") 'next-buffer)
(global-set-key (kbd "C-`") 'previous-buffer)

;;keyss
(global-set-key (kbd "s-e") (lambda () (interactive) (previous-buffer)))
(global-set-key (kbd "s-r") (lambda () (interactive) (next-buffer)))

;; line numbers
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; old deprecated 
;; (global-linum-mode 1)

;; disable visi-bell completely
(setq ring-bell-function 'ignore)


;; no tabs
(setq c-basic-indent 4)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; font
;; (set-frame-font "Anonymous Pro-16")

;; scrolling
(setq scroll-step 1
   scroll-conservatively 10000)

;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; parens
(show-paren-mode 1)

;; Warn before you exit emacs!
(setq confirm-kill-emacs 'yes-or-no-p)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)


;; fontss
(setq-default line-spacing 2)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14:width=normal"))

;; no need toolbar scrollbar title and make transparent window
(tool-bar-mode 0)
(scroll-bar-mode 0) 
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(alpha 85 85))
;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" default))
 '(package-selected-packages
   '(dante flycheck-color-mode-line flycheck-pos-tip flycheck company popup seti-theme color-theme popwin evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
