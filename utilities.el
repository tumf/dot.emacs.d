;; (Utilities) ;;
(push "~/.emacs.d/utilities" load-path)

;;jaspace
(require 'jaspace)

;; AutoSaveBuffersEnhanced
(push "~/.emacs.d/utilities/auto-save-buffers-enhanced" load-path)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:"))


;; yasnippets
(require 'yasnippet)
(setq yas-snippet-dirs
      '(
        "~/.emacs.d/el-get/yasnippet/snippets"
        "~/.emacs.d/el-get/rspec-mode/snippets"
        ))

(yas/global-mode 1)


;; anything
(require 'anything-config)
;;(require 'anything-c-yasnippet)
(global-set-key [?\C-;] 'anything)
(setq anything-sources
     (list
      anything-c-source-buffers
      anything-c-source-imenu
      ;;anything-c-source-yasnippet
      anything-c-source-files-in-current-dir
      anything-c-source-recentf
      anything-c-source-file-name-history
      anything-c-source-emacs-commands
      ))

