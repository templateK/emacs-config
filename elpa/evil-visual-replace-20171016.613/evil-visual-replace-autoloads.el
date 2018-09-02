;;; evil-visual-replace-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-visual-replace" "evil-visual-replace.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-visual-replace.el

(autoload 'evil-visual-replace-visual-bindings "evil-visual-replace" "\
Set up evil-visual-replace default key bindings.

Bind `evil-visual-replace-query-replace' to M-% and
`evil-visual-replace-replace-regexp' to C-M-% in `evil-visual-state-map'.

If the optional parameter USE-PCRE is non-nil, C-M-% is instead bound to
`evil-visual-replace-pcre-query-replace'.

\(fn &optional USE-PCRE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-visual-replace" '("evil-visual-replace-pcre-replace-regexp")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-visual-replace-autoloads.el ends here
