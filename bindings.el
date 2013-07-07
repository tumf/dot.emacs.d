;; (Key Bindings) ;;

(setq-default tab-width 2 indent-tabs-mode nil)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-w" 'kill-region)

;; for Mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

