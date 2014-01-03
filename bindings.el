;; (Key Bindings) ;;

(setq-default tab-width 2 indent-tabs-mode nil)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cg" 'magit-status)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-w" 'kill-region)

(global-set-key (kbd "C-c v") 'magit-status)

;; ウインドウを表示する
;; http://openlab.dino.co.jp/2007/11/20/110609148.html
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; for Mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))
