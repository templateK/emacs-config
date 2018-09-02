(load "~/.emacs.d/my-packages.el")

(require 'use-package)

(use-package color-theme
  :init
  :config
  ;; (set-frame-parameter nil 'background-mode 'dark)
  ;; (set-terminal-parameter nil 'background-mode 'dark)
  (load-theme 'gruvbox-dark-hard t)
  ;; (load-theme 'habamax t)
 ) ;; end of color-theme

(use-package magit
  :init
  :config
  ;; (global-set-key (kbd "s-g") 'magit-status)
  (evil-leader/set-key "gs" 'magit-status)
 ) ;; end of magit

(use-package popwin
  :init
  (setq popwin:popup-window-height 26)
  :config
  (global-set-key (kbd "s-p") 'popwin:close-popup-window)
  (push '(flycheck-error-list-mode 
          :dedicated t 
          :position bottom 
          :stick t 
          :noselect t) 
        popwin:special-display-config)
  (popwin-mode 1)
 ) ;; end of popwin

;; COMPLETION
(add-hook 'after-init-hook 'global-company-mode)
;shortcut for completion
(global-set-key (kbd "C-c w") 'company-complete)
;after how many letters do we want to get completion tips? 1 means from the first letter
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase 0)
;after how long of no keys should we get the completion tips? in seconds
(setq company-idle-delay 0.4)


(use-package flycheck-color-mode-line
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  :config
 ) ;; end of flycheck-color-mode-line

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :init
;;   :config
;;   (flycheck-pos-tip-mode)
;;  ) ;; end of flycheck-pos-tip


;; ENVironment ;;
(use-package direnv
  :config
  (setq direnv-always-show-summary t
        direnv-show-paths-in-summary nil)
  (global-set-key (kbd "M-z") 'direnv-mode)
 ) ;; end of direnv


; HASKELL ;;
(use-package haskell-mode
  :init
  :commands 'haskell-mode
  :config
  ;a few convenient shortcuts
  (define-key haskell-mode-map (kbd "C-c C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-l C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  ; search work under the cursor in hoogle
  (define-key haskell-mode-map (kbd "C-:") 'haskell-search-hoogle)
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)  
 ) ;; end of haskell-mode

(use-package dante
  :after haskell-mode
  :ensure t
  :commands 'dante-mode
  :init
  :config
  (evil-leader/set-key ",a" 'dante-type-at)
  ;;(add-hook 'dante-mode-hook 'dante-restart)
  ) ;; end of dante

;; ERRORS ON THE FLY
(use-package flycheck
  :after (haskell-mode popwin)
  :commands 'flycheck-buffer
  :init
  ;; toggle flycheck window
  (defun spacemacs/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
        (quit-window nil window)
      (flycheck-list-errors)))
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-set-key [f9] 'spacemacs/toggle-flycheck-error-list)
  ) ;; end of flycheck



(let ((filename "/Users/taemu/.emacs.d/native/libemacs-dyn-cabal.dylib"))
  (if (and (file-exists-p filename) (file-executable-p filename))
    (progn
      (module-load filename)
      (setq-default dante-repl-command-line-methods-alist
          `((bare  . ,(lambda (root) 
            '("cabal" "repl" 
              (emacs-dyn-cabal-target (haskell-cabal-find-file) default-directory) 
              "--builddir=dist/dante"))))))
  (setq-default dante-repl-command-line-methods-alist
        `((bare  . ,(lambda (root) 
          '("cabal" "repl" dante-target "--builddir=dist/dante")))))
 )) ;; end of if let

 
;; default is firefox. Change this if you want to open hoogle on a different browser.
;; (setq browse-url-generic-program (executable-find "firefox"))

(defun haskell-search-hoogle ()
   "Search hoogle for the word under the cursor"
   (interactive)
   (browse-url-generic (concat "https://hoogle.haskell.org/?hoogle=" (thing-at-point 'word))))

(use-package evil
  :ensure t
  :init
  (global-set-key (kbd "s-j") 'evil-scroll-down)
  (global-set-key (kbd "s-k") 'evil-scroll-up)
  (global-set-key (kbd "s-b") 'evil-delete-buffer)
  :config 
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-visual-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-visual-state-map (kbd "TAB") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (evil-mode 1)
 ) ;; end of evil

(use-package evil-visual-replace
  :after evil
  :init
  :config
    (evil-visual-replace-visual-bindings)
  ) ;; end of evil-visual-replace

(use-package evil-leader
  :after (evil magit)
  :init
  :config
  (setq-default evil-leader/in-all-states t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "ff" 'find-file
   "fd" 'evil-delete-buffer
   "fs" 'switch-to-buffer
   "wd" 'evil-window-delete
   "wh" 'evil-window-left
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "wc" 'olivetti-mode
   "w1" 'delete-other-windows
   "ad" 'dired
   )
   (evil-leader/set-key-for-mode 'dante-mode "a" 'dante-type-at)
  (global-evil-leader-mode) 
 ) ;; end of evil-leader

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "cl" 'evilnc-comment-or-uncomment-lines)
  ) ;; end of evil-nert-commenter

(use-package evil-magit
  :after magit
  :init
  :config
  ;;(evil-magit-use-y-for-yank)
  ) ;; end of evil-magit

(use-package olivetti
  :ensure t
  :init
  (setq olivetti-body-width 120)
  (setq olivetti-hide-mode-line t)
  (setq olivetti-hide-line-numbers t)
  :config
 ) ;; end of olivetti
