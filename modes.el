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

;;(add-hook 'rspec-mode-hook
;;          '(lambda ()
;;             (local-set-key "\C-c\C-c" 'rspec-verify-single)))
;;(eval-after-load 'rspec-mode '(rspec-install-snippets))
;;(yas/load-directory "~/.emacs.d/el-get/rspec-mode/snippets")
