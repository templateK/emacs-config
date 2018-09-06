;; disable automatic description as this is both annoying and can easily
;; get intero stuck
(global-eldoc-mode -1)

(add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines nil)))

(setq resize-mini-windows t) ; grow and shrink as necessary
(setq max-mini-window-height 10) ; grow up to max of 10 lines

(setq minibuffer-scroll-window t)

;; will search for cabal in these directories
;; (add-to-list 'exec-path "/usr/local/bin")
;; (add-to-list 'exec-path "~/.local/bin")

;; load packages
;; (load "~/.emacs.d/my-loadpackages.el")

;;keyg
;; non-evilified buffer keybindings
(global-set-key (kbd "s-'") 'other-window)
(global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)          ; move to lower window
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-M-<tab>") 'previous-buffer)
(global-set-key (kbd "s-b") 'kill-this-buffer)            ; kill current buffer without asking
(global-set-key (kbd "s-w") 'switch-to-buffer)
(global-set-key (kbd "C-c a") 'org-agenda)

;; auto indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;; easy escaping from stale state
;; somewhat works. but maybe below is better.
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; attemp to save orignal ESC functionality while support remap ESC to C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


;; line numbers
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; disable visi-bell completely
(setq ring-bell-function 'ignore)

;; no tabs
(setq c-basic-indent 4)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; dired
;; this is disabled because it feels sluggish.
;; (setq ls-lisp-use-insert-directory-program t)
;; (setq insert-directory-program "/usr/local/bin/gls")
;; (setq dired-listing-switches "-alXGh --group-directories-first")

;; scrolling
(setq scroll-step 1 scroll-conservatively 10000)

;; parens
(show-paren-mode 1)

;; Warn before you exit emacs!
(setq confirm-kill-emacs 'yes-or-no-p)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)

;; force save for flycheck and other purpose.
(defun save-buffer-always ()
  "Save current buffer in visited file even if it has not been modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(define-key global-map (kbd "s-s") 'save-buffer-always)

;; fontss
(setq-default line-spacing 2)
(add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK KR-14:width=normal"))

;; framess
(tool-bar-mode 0)
(scroll-bar-mode 0) 
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(alpha 85 85))


(require 'package) ;; archss
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar required-packages
  '(;; packss
    use-package
    evil evil-leader evil-visual-replace evil-nerd-commenter
    expand-region
    popwin
    ;; popup
    flycheck flycheck-color-mode-line
    ;; flycheck-pos-tip
    company dante direnv
    gruvbox-theme habamax-theme
    magit evil-magit
    org
    ) ;; end registered packages
   "a list of packages to ensure are installed at launch.") ;; end of defvar


;; check all packages installed and if not, install it.
(require 'cl-lib)

(defun packages-installed-p ()
  (cl-loop for p in required-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; start of use-package
(require 'use-package)

;; custom packages
(add-to-list 'load-path "~/.emacs.d/custom/hlint")

(use-package hs-lint
  :after compile
  :init
  :config
  ) ;; end of hs-hlint


;; themess
(use-package color-theme
  :init
  :config
  ;; (set-frame-parameter nil 'background-mode 'dark)
  ;; (set-terminal-parameter nil 'background-mode 'dark)
  (load-theme 'gruvbox-dark-hard t)
  ;; (load-theme 'habamax t)
 ) ;; end of color-theme

;; COMPLETION
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-c w") 'company-complete)
;after how many letters do we want to get completion tips? 1 means from the first letter
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0.4)


;; flyss
(defun toggle-flycheck-error-list ()
"Toggle flycheck's error list window.If the error list is visible, hide it.  Otherwise, show it."
(interactive)
(-if-let (window (flycheck-get-error-list-window))
    (quit-window nil window)
    (flycheck-list-errors)))

(use-package flycheck
  :after haskell-mode
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-check-syntax-automatically '(save mode-enable))
  :config
  (add-hook 'flycheck-mode-hook 'dante-mode)
 ) ;; end of flycheck

;; (use-package flycheck-color-mode-line
;;   :after flycheck
;;   :init
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;;   :config
;;  ) ;; end of flycheck-color-mode-line

;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :init
;;   :config
;;   (flycheck-pos-tip-mode)
;;  ) ;; end of flycheck-pos-tip

;; hass
;; default is firefox. Change this if you want to open hoogle on a different browser.
(setq browse-url-generic-program (executable-find "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

;; dantess
(use-package dante
  :after (direnv flycheck)
  :ensure t
  :init
  :config
  (let ((filename "/Users/taemu/.emacs.d/native/libemacs-dyn-cabal.dylib"))
    (if (and (file-exists-p filename) (file-executable-p filename))
        (progn
          (module-load filename)
          (fset 'dante-target (lambda () (emacs-dyn-cabal-target (haskell-cabal-find-file) (buffer-file-name))))
          (setq-default dante-repl-command-line '("cabal" "repl" (dante-target) "--builddir=dist/dante")))
      (setq-default dante-repl-command-line '("cabal" "repl" dante-target "--builddir=dist/dante"))
      ;; (setq-default dante-repl-command-line-methods-alist `((bare  . ,(lambda (root) '("cabal" "repl" dante-target "--builddir=dist/dante")))))
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
 ))) ;; end of dante

(use-package haskell-mode
  :init
  (defun haskell-search-hoogle ()
    "Search hoogle for the word under the cursor"
    (interactive)
    (browse-url-generic (concat "https://hoogle.haskell.org/?hoogle=" (thing-at-point 'word))))
  :commands 'haskell-mode
  :config
  ;a few convenient shortcuts
  ;; (define-key haskell-mode-map (kbd "C-c C-`") 'haskell-interactive-bring)
  ;; (define-key haskell-mode-map (kbd "C-l C-l") 'haskell-process-load-or-reload)
  ;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  ; search work under the cursor in hoogle
  ;; (define-key haskell-mode-map (kbd "C-:") 'haskell-search-hoogle)
  (add-hook 'haskell-mode-hook 'flycheck-mode)  
 ) ;; end of haskell-mode

;; envss
(use-package direnv
  :config
  (setq direnv-always-show-summary t
        direnv-show-paths-in-summary nil)
  (global-set-key (kbd "M-z") 'direnv-mode)
 ) ;; end of direnv

;; popss
(use-package popwin
  :init
  (setq popwin:popup-window-height 26)
  (global-set-key (kbd "s-p") 'popwin:close-popup-window)
  :config
  (push '(flycheck-error-list-mode
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)
  (popwin-mode 1)
 ) ;; end of popwin

;; evilss
(use-package evil
  :after evil-leader
  :ensure t
  :init
  (global-set-key (kbd "s-j") 'evil-scroll-down)
  (global-set-key (kbd "s-k") 'evil-scroll-up)
  :config 
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-visual-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-visual-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-visual-state-map (kbd "TAB") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)

  ;; https://github.com/cofi/evil-leader/issues/31
  ;; but, it's not working for haskell-cabal-mode
  ;; (evil-set-initial-state 'haskell-cabal-mode 'motion)
  (evil-mode 1)
 ) ;; end of evil

(use-package evil-leader
  :init
  ;; easy user init file editing
  (defun find-user-init-file ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (find-file-other-window user-init-file))
  (defun reload-user-init-file ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (load-file user-init-file))
  (setq-default evil-leader/in-all-states t)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; keyss
    ;; flycheck
   "ee" 'flycheck-mode
   "el" 'toggle-flycheck-error-list
   "en" 'flycheck-next-error
   "e;" 'flycheck-previous-error
   ;; hs-lint
   "l" 'hs-lint
   ;; expand-region
   "v" 'er/expand-region
   ;; evil-nerd-commenter
   "cl" 'evilnc-comment-or-uncomment-lines
   ;; magit
   "gs" 'magit-status

   "fs" 'find-file
   "fd" 'evil-delete-buffer
   "ff" 'switch-to-buffer
   "wd" 'evil-window-delete
   "wh" 'evil-window-left
   "wj" 'evil-window-down
   "wk" 'evil-window-up
   "wl" 'evil-window-right
   "wc" 'olivetti-mode
   "w1" 'delete-other-windows
   "ad" 'dired
   "fed" 'find-user-init-file
   "qq" 'save-buffers-kill-emacs
   ) ;; end of set-key

  ;; mode specifiy evil-leader keys
  ;; (evil-leader/set-key-for-mode 'dired-mode "k" 'dired-up-directory)
  (evil-leader/set-key-for-mode 'haskell-mode
    ;; TODO: dante inserting type at
    ;; "m" 'dante-blah
    ";" 'dante-type-at
    "h" 'haskell-hoogle-lookup-from-local)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

  (global-evil-leader-mode) 
 ) ;; end of evil-leader

(use-package evil-visual-replace
  :after evil
  :init
  :config
  (evil-visual-replace-visual-bindings)
 ) ;; end of evil-visual-replace

;; regionss
(use-package expand-region
  :after evil
  :init
  (setq expand-region-contract-fast-key ";")
  :config
  ) ;; end of expand-region

(use-package evil-nerd-commenter
  :init
  :config
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

(use-package magit
  :init
  :config
 ) ;; end of magit

(custom-set-variables)
(custom-set-faces)
