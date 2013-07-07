;; (Key Bindings) ;;

(setq-default tab-width 2 indent-tabs-mode nil)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-w" 'kill-region)

;; ウインドウを表示する
;; http://openlab.dino.co.jp/2007/11/20/110609148.html
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; for Mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

