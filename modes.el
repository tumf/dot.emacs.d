;; (Modes) ;;

;; rspec
(add-hook 'rspec-mode-hook
          '(lambda ()
             (local-set-key "\C-c\C-c" 'rspec-verify-single)))

