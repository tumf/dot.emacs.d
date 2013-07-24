;; (Modes) ;;

;; Ruby
(autoload 'ruby-mode "ruby-mode"  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))

;; rspec
(require 'rspec-mode)
(load-library (concat "~/.emacs.d/utilities" "/yasnippets-rspec/setup.el"))
(global-set-key "\C-c\C-c" 'rspec-verify-single)

 ;;; rhtml-mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

;;(add-hook 'rspec-mode-hook
;;          '(lambda ()
;;             (local-set-key "\C-c\C-c" 'rspec-verify-single)))
;;(eval-after-load 'rspec-mode '(rspec-install-snippets))
;;(yas/load-directory "~/.emacs.d/el-get/rspec-mode/snippets")
(defun php-indent-hook()
  (c-set-style "stroustrup")
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  ;;(c-set-offset 'case-label '+) ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧
(add-hook 'php-mode-hook 'php-indent-hook)
