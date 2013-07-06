;; (Utilities) ;;

;; AutoSaveBuffersEnhanced
(push "~/.emacs.d/utilities/auto-save-buffers-enhanced" load-path)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

;; anything
(require 'anything-config)
(global-set-key [?\C-;] 'anything)








