;;; package --- Summary
;;; Commentary:
;;; Code:

;; my packages to be install
(defvar rootpath (expand-file-name "~/.emacs.d"))
(setq load-path (cons (concat rootpath "/elisp")load-path))

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; submodule関連
(defvar elisp-package-dir (concat rootpath "/elisp"))
(defun submodule (dir)
  (push (format "%s/%s" elisp-package-dir dir) load-path))
(defun require-submodule (name &optional dir)
  (push (format "%s/%s" elisp-package-dir (if (null dir) name dir)) load-path)
  (require name))

;; Set `SHELL' environment variable
(setenv "SHELL"
        (or (executable-find "zsh")
            (executable-find "bash")
            (error "Cannot find executable shells")))

;; Set `PATH' environment variable and `exec-path'
(setenv "PATH"
        (replace-regexp-in-string
         "[ \t\n]*$" ""
         (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'")))

(setq exec-path (split-string (getenv "PATH") path-separator))
(defvar my-packages '(
                      better-defaults
                      ;;exec-path-from-shell
                      super-save
                      paredit
                      idle-highlight-mode
                      editorconfig
                      ido-ubiquitous
                      find-file-in-project
                      magit
                      ag
                      smex
                      scpaste
                      ;;wakatime-mode
                      helm helm-descbinds helm-ag helm-git helm-c-yasnippet helm-projectile
                      markdown-mode
                      smart-newline
                      flycheck
                      nodejs-repl
                      multi-term shell-pop
                      color-theme
                      yaml-mode
                      json-mode
;;                      auto-save-buffers-enhanced
                      auto-complete
                      web-mode
                      jade-mode
                      php-mode php-completion
                      neotree
                      ack
                      magit-gitflow
                      yasnippet
                      ansible-doc
                      rvm
                      projectile projectile-rails
                      py-autopep8 pyenv-mode
                      dockerfile-mode
                      smartrep
                      multiple-cursors
                      github-issues
                      rubocop
                      expand-region
                      ))

(require 'package) ;; You might already have this line
;;(package-refresh-contents)


(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(package-install 'better-defaults)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(exec-path-from-shell-initialize)

;; ido-mode をオフにする
(ido-mode nil)

;;
;; 文字コードの設定
;;
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;;editorconfig
(editorconfig-mode 1)

;;(cond
;; ((or (eq window-system 'mac) (eq window-system 'ns))
;;  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱うため以下の設定をする必要がある
;;  (require 'ucs-normalize)
;;  (setq file-name-coding-system 'utf-8-hfs)
;;  (setq locale-coding-system 'utf-8-hfs))
;; (or (eq system-type 'cygwin) (eq system-type 'windows-nt)
;;     (setq file-name-coding-system 'utf-8)
;;     (setq locale-coding-system 'utf-8)
;;     ;; もしコマンドプロンプトを利用するなら sjis にする
;;     ;; (setq file-name-coding-system 'sjis)
;;     ;; (setq locale-coding-system 'sjis)
;;     ;; 古い Cygwin だと EUC-JP にする
;;     ;; (setq file-name-coding-system 'euc-jp)
;;     ;; (setq locale-coding-system 'euc-jp)
;;     )
;; (t
;;  (setq file-name-coding-system 'utf-8)
;;  (setq locale-coding-system 'utf-8)))

;; 外部コマンドのPATH
;; この設定が利用する主なコマンド `git` `ag`
(push "/usr/local/bin" exec-path)
(push "~/.pyenv/shims/" exec-path)

;; Key Bindings
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-w" 'kill-region)
(define-key global-map "\C-\\" nil) ; \C-\の日本語入力の設定を無効にする

(setq-default tab-width 2 indent-tabs-mode nil)

;; ウインドウを表示する
;; http://openlab.dino.co.jp/2007/11/20/110609148.html
(global-set-key "\M-2" 'make-frame)
(global-set-key "\M-0" 'delete-frame)

;; beepを無視する
(setq ring-bell-function 'ignore)

;; for Mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(require 'server)
(or (server-running-p)
    (server-start))
(set-language-environment 'Japanese)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; Do not show the splash screen or message
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show row and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Display the current time and system load
(load-library "time")
(setq display-time-24hr-format t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)
(blink-cursor-mode t)

;;行番号表示
(require 'linum)
(global-linum-mode)

;;画面サイズ
(setq initial-frame-alist
      '((top . 1) (left . 1) (width . 160) (height . 50)))

;; from http://sakito.jp/emacs/emacs23.html
(when (display-graphic-p)
  ;; color-theme
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-midnight)
  ;; Font
  (create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
                    'unicode
                    (font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
                    nil
                    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
)
;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;; Remove all the mouse-assisted crud
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Highlighting "TODO", "FIXME" and friends
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
                                       1 font-lock-warning-face t)))))


;; Show matching parenthesis
(show-paren-mode t)


;; Set random parameters
(setq search-highlight t        ; Highlight all visible matches
      query-replace-highlight t ; Highlight all visible matches
      transient-mark-mode t)    ; Perform certain commands only on the marked region


;; Properly display colors in shell
(ansi-color-for-comint-mode-on)


;; Set a better font if running in windows
(if (eq system-type 'windows-nt)
    (set-frame-font "consolas-10"))



;;;;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)

;; helm-mode
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(setq helm-yas-display-key-on-candidate t)

(helm-descbinds-mode)

(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-.") 'helm-yas-complete)
(global-set-key (kbd "C-c b") 'helm-descbinds)
(global-set-key (kbd "C-s") 'helm-occur)

(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (if (ignore-errors (projectile-project-root))
                                    (helm-do-ag-project-root)
                                  (helm-do-grep-ag))))

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-r") 'helm-resume)

(global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
(global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
(global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
(global-set-key (kbd "C-c o p") 'helm-open-github-from-pull-requests)

(define-key helm-map (kbd "C-k") 'helm-buffer-run-kill-buffers)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)


(setq recentf-max-saved-items nil)

(setq helm-mini-default-sources
 '(
   helm-source-buffers-list
   helm-source-recentf
   helm-source-files-in-current-dir
   ))

;;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; magit-mode
(require 'magit)
(global-set-key (kbd "C-'")
                (lambda ()
                  (interactive)
                  (save-buffer)
                  (magit-status)))

;; wakatime-mode
;;(global-wakatime-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(if (file-readable-p (expand-file-name "~/.local.el"))
    (load-file (expand-file-name "~/.local.el"))
  nil)

;;
;; Ruby mode
;;

(global-auto-revert-mode 1)

;; ruby modeで C-x C-s をrobocopによる整形に割り当てる
(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-x C-s")
     (lambda ()
       (interactive)
       (save-buffer)
       (rubocop-autocorrect-current-file))))

;; flycheck
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))

(require 'helm-rdefs)
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "C-c r") 'helm-rdefs)))

(eval-after-load 'rspec-mode
  '(define-key rspec-mode-map (kbd "C-c C-c")
     (lambda ()
       (interactive)
       (save-buffer)
       (rspec-verify-single))))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; C-x C-q でrspecの結果からpryへ
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(require 'smart-compile)
(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c C-c") 'smart-compile))


(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; rvm-mode
(rvm-use-default)
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(setenv "PAGER" "cat")
;;(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)


;; smart-newline
(add-hook 'ruby-mode-hook ;; or any major-mode-hooks
  (lambda ()
    (smart-newline-mode t)))



;;
;; file-term
;;
(require 'multi-term)
(setq system-uses-terminfo nil)
(setq shell-file-name "/bin/zsh")
(setq multi-term-program shell-file-name)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)
;;(global-set-key (kbd "C-c t") '(lambda ()
;;                                (interactive)
;;                                (multi-term)))

;;
;; shell-pop
;;
(require 'shell-pop)

;;
;; auto-insert
;; http://d.hatena.ne.jp/higepon/20080731/1217491155
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/insert/")
(setq auto-insert-alist
      (nconc '(
               ("\\.rb$" . "template.rb")
               ("\\.php$" . "template.php")
               ("\\.py$" . "template.py")
               ("\\.ya?ml$" . "template.yaml")
               ("Dockerfile" . "Dockerfile")
               ) auto-insert-alist))

;; 空白制御
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
(setq whitespace-action '(delete-trailing-whitespace-except-current-line))
(global-whitespace-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)

;; AutoSaveBuffersEnhanced
;;(require 'auto-save-buffers-enhanced)
;;(auto-save-buffers-enhanced t)
;;(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "/sudo:" "/multi:" "COMMIT_EDITMSG"))
;;(setq auto-save-buffers-enhanced-interval 2)

;; Super save mode
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
;; save-buffer when switch buffer.
;;(add-hook 'switch-buffer-functions
;;          (lambda (prev cur)
;;            (save-buffer prev)))



;;ファイルの最後に改行を挿入する
(setq require-final-newline t)

;; auto-complete
(ac-config-default)

(setq ac-use-menu-map t)
(global-auto-complete-mode t)
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))


;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$"     . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-words-in-buffer ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))
          ("php" . (ac-source-php-completion
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-dictionary))))

  (setq web-mode-engines-alist
        '(("php"    . "\\.php\\'")
          ("blade"  . "\\.blade\\."))))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;Window Move M-up/down/left/right
(windmove-default-keybindings 'meta)
;;; neotree
(require 'neotree)
(global-set-key [f9] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)


(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;; ファイル名補完キーマップで?をそのまま入力できるようにする
;;;(define-key minibuffer-local-filename-completion-map (kbd "?") nil)
;;; ffapでワイルドカードを指定するとdiredを開くようにする
;;;(setq ffap-pass-wildcards-to-dired t)
;;; C-x C-fなどをffap関係のコマンドに割り当てる
;;;(ffap-bindings)

;; ansible minor mode
(require-submodule 'ansible "ansible")
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;; ansible doc
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

;; jinja2-mode
(add-to-list 'auto-mode-alist '("\\.j2$"     . jinja2-mode))

(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)

(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; python-mode
(require 'py-autopep8)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map
              (kbd "C-x C-s")
              (lambda () (interactive)
                (py-autopep8)
                (save-buffer)))))
(pyenv-mode-set "2.7.10")

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

;; php-mode
(add-to-list 'auto-mode-alist '("\\.php$"     . php-mode))

;; Dockerfile
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(defvar ctl-q-map (make-keymap))
(define-key global-map "\C-q" ctl-q-map)

(require 'smartrep)
(smartrep-define-key
    global-map "C-q" '(("n" . (lambda () (scroll-other-window 1)))
                       ("p" . (lambda () (scroll-other-window -1)))
                       ("N" . 'scroll-other-window)
                       ("P" . (lambda () (scroll-other-window '-)))
                       ("a" . (lambda () (beginning-of-buffer-other-window 0)))
                       ("e" . (lambda () (end-of-buffer-other-window 0)))))

(require 'multiple-cursors)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
(global-unset-key "\C-t")
(smartrep-define-key
    global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

;;;
;;; org-mode
;;;
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; org-modeの初期化
(require 'org-install)
;; キーバインドの設定
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-'") nil)
     ))

;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")

;; inline表示の画像をC-cC-cの後に自動更新
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))

;; TODO状態
(setq org-todo-keywords
      '((sequence "PLAN(p)" "TODO(t)" "WIP(w)" "|" "DONE(d)" "PEND(e)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

(setq open-junk-file-format "~/memo/%Y%m%d.org")
(global-set-key "\C-xj" 'open-junk-file)

(require 'tramp)
(setq tramp-default-method "ssh")

(defun git-now ()
  "Execute git now."
  (interactive)
  (shell-command (concat "git now")))

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))
;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;
