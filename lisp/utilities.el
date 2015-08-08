;; (Utilities) ;;
(push "~/.emacs.d/utilities" load-path)

;; dirtree
;;(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(setq  dirtree-windata '(frame left 0.16 nil))

(require 'dirtree)

(defmacro my/if-dirtree-window-exist (win &rest on-t on-nil)
  `(let* ((buffer (get-buffer dirtree-buffer))
          (,win (and buffer
                    (get-buffer-window buffer))))
         (if ,win
             ,@on-t
           ,@on-nil)))

(global-set-key [f8]
                #'(lambda ()
                   (interactive)
                   (my/if-dirtree-window-exist window
                                               (delete-window window)
                                               (dirtree default-directory nil))))

(defun my/dirtree-update ()
  (my/if-dirtree-window-exist window
                              (unless (equal (buffer-name) dirtree-buffer)
                                (dirtree default-directory nil))))

(defadvice switch-to-buffer (after dirtree-update (buffer-or-name
                                                   &optional
                                                   norecord
                                                   force-same-window))
  (my/dirtree-update))
(defadvice quit-window (after dirtree-update (&optional kill window))
  (my/dirtree-update))
(defadvice windmove-do-window-select (after dirtree-update (dir &optional arg window))
  (my/dirtree-update))

(ad-activate 'switch-to-buffer)
(ad-activate 'quit-window)
(ad-activate 'windmove-do-window-select)

;;jaspace
;; (require 'jaspace)

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

;;;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
;; (setq whitespace-action '(auto-cleanup))
(defun delete-trailing-whitespace-except-current-line ()
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)))
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region (1+ end) (point-max))
          (delete-trailing-whitespace))))))
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)
(global-whitespace-mode 1)

;;(defvar my/bg-color "#232323")
;;(set-face-attribute 'whitespace-trailing nil
;;                    :background my/bg-color
;;                    :foreground "DeepPink"
;;                    :underline t)
;;(set-face-attribute 'whitespace-tab nil
;;                    :background my/bg-color
;;                    :foreground "LightSkyBlue"
;;                    :underline t)
;;(set-face-attribute 'whitespace-space nil
;;                    :background my/bg-color
;;                    :foreground "GreenYellow"
;;                    :weight 'bold)
;;(set-face-attribute 'whitespace-empty nil
;;                    :background my/bg-color)
;;

;; AutoSaveBuffersEnhanced
(push "~/.emacs.d/utilities/auto-save-buffers-enhanced" load-path)
(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)
(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:" "COMMIT_EDITMSG"))
(setq auto-save-buffers-enhanced-interval 2)

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
      anything-c-source-emacs-commands))


;; yasnippets
(require 'yasnippet)
;;(setq yas-snippet-dirs
;;      '(
;;        "~/.emacs.d/snippets"
;;        ))
(setq yas-snippet-dirs
      '(
        "~/.emacs.d/snippets"
        "~/.emacs.d/el-get/yasnippet/snippets"
        ;;"~/.emacs.d/el-get/rspec-mode/snippets"
        ;;"~/.emacs.d/snippets/rails/rails-snippets"
        ))
(yas/global-mode 1)

;; auto-complete
;;(require 'auto-complete-config)
;;(global-auto-complete-mode 1)
(when (require 'auto-complete-config nil t)
  (ac-config-default)
  (setq ac-auto-start 1)
  (setq ac-dwim t)
  (setq ac-use-menu-map t) ;; C-n/C-pで候補選択可能
  ;;(delq 'ac-source-yasnippet ac-sources)
  (add-to-list 'ac-sources 'ac-source-yasnippet) ;; 常にYASnippetを補完候補に
  (setq ac-dictionary-directories "~/.emacs.d/work/auto-complete/ac-dict") ;; 辞書ファイルのディレクトリ
  (setq ac-comphist-file "~/.emacs.d/work/auto-complete/ac-comphist.dat") ;; 補完履歴のキャッシュ先
)
;;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
(setf (symbol-function 'yas-active-keys)
      (lambda ()
        (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))




(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
