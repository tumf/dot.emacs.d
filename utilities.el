;; (Utilities) ;;

;; AutoSaveBuffersEnhanced
(push "~/.emacs.d/utilities/auto-save-buffers-enhanced" load-path)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

;; anything
(require 'anything-config)
(global-set-key [?\C-;] 'anything)
(setq anything-sources
     (list
      anything-c-source-imenu
      anything-c-source-files-in-current-dir
      anything-c-source-buffers
      anything-c-source-recentf
      anything-c-source-file-name-history
      ))








