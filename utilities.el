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


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
