(require 'package) ;; You might already have this line

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;(push "/usr/local/bin" exec-path)
;;https://github.com/dholm/dotemacs/blob/master/.emacs.d/init.el
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 160) (height . 60)))
(cd "~/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-user-package-directory "~/.emacs.d/init")
(setq el-get-sources
      '(;; Code helpers
        ;;auto-complete cedet dtrt-indent ecb flymake-cursor
        ;;google-c-style smart-tab ensime scion pymacs jedi perl-completion
        google-c-style
        auto-complete
        auto-complete-ruby
        yasnippet auto-complete-yasnippet
        neotree
        ;; Modes
        ruby-mode rvm haml-mode rhtml-mode
        ruby-electric
        smart-newline
        php-mode
        markdown-mode
        yaml-mode
        multiple-cursors smartrep
        sass-mode
        ;;gnuplot-mode haskell-mode js2-mode markdown-mode nxhtml rainbow-mode
        ;;wc-mode python-mode pylookup slime php-mode scala-mode2 cperl-mode
        ;;jinja rvm

        ;; Version Control Systems
        magit
        ;;vc-clearcase git-gutter-fringe

        ;; Utilities
        ;;deft lusty-explorer profile-dotemacs sunrise-commander elim perspective
        shell-command
        ack
        ;;emacs-w3m

        ;; Editing
        ;;multiple-cursors expand-region undo-tree browse-kill-ring
        ;;fill-column-indicator

        ;; Navigation
        ;;ace-jump-mode jump-char minimap
        anything

        ;; Themes
        color-theme-solarized

        ;; dirtree
        ;;dirtree
        tree-mode
        windata
        popwin
	))
(el-get 'sync el-get-sources)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;;(push "~/.emacs.d" load-path)
(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/bin" exec-path)



;; popwinの設定
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)


(load "emacs.el")
(load "modes.el")
(load "utilities.el")
(load "bindings.el")
(load "themes.el")

;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(if (file-readable-p (expand-file-name "~/.emacs.local"))
    (load-file (expand-file-name "~/.emacs.local"))
  nil)


;; trampの設定: /sudo:
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" nil nil))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))
