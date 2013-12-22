;; (Utilities) ;;
(push "~/.emacs.d/utilities" load-path)

;;jaspace
(require 'jaspace)

(require 'saveplace)
(setq-default save-place t)

;;ファイルの最後に改行を挿入する
(setq require-final-newline t)

(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
;; (setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)
(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

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
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
