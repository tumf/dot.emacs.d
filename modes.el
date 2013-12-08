;; (Modes) ;;

;; Ruby
(autoload 'ruby-mode "ruby-mode"  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile$" . ruby-mode))

;; chef
(push "~/.emacs.d/modes/chef-mode" load-path)
(require 'chef-mode)

;; multi-mode
(load-library "~/.emacs.d/modes/php-htm-mode/multi-mode.el")
(load-library "~/.emacs.d/modes/php-htm-mode/php-htm-mode.el")

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
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

(defun php-indent-hook()
  (c-set-style "stroustrup")
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  ;;(c-set-offset 'case-label '+) ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧
(add-hook 'php-mode-hook 'php-indent-hook)
