;; my packages to be install
(defvar my-packages '(
                      better-defaults
                      paredit
                      idle-highlight-mode
                      ido-ubiquitous
                      find-file-in-project
                      magit
                      smex
                      scpaste
                      wakatime-mode
                      helm helm-descbinds helm-ag helm-git
                      markdown-mode
                      smart-newline
                      flycheck
                      php-mode
                      nodejs-repl
                      multi-term shell-pop
                      color-theme
                      yaml-mode
                      ;;yasnippet yasnippet-bundle
                      ;;ansible
                      ))

(package-refresh-contents)
(require 'package) ;; You might already have this line

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(package-install 'better-defaults)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; 文字コードの設定
;;
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

(cond
 ((or (eq window-system 'mac) (eq window-system 'ns))
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱うため以下の設定をする必要がある
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (or (eq system-type 'cygwin) (eq system-type 'windows-nt)
     (setq file-name-coding-system 'utf-8)
     (setq locale-coding-system 'utf-8)
     ;; もしコマンドプロンプトを利用するなら sjis にする
     ;; (setq file-name-coding-system 'sjis)
     ;; (setq locale-coding-system 'sjis)
     ;; 古い Cygwin だと EUC-JP にする
     ;; (setq file-name-coding-system 'euc-jp)
     ;; (setq locale-coding-system 'euc-jp)
     )
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

;; 外部コマンドのPATH
;; この設定が利用する主なコマンド `git` `ag`
(push "/usr/local/bin" exec-path)

;; Key Bindings
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cg" 'magit-status)
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
    (set-default-font "consolas-10"))


;; helm-mode
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds)

(helm-descbinds-mode)

(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c b") 'helm-descbinds)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c s") 'helm-ag)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)

;;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; magit-mode
(require 'magit)
(global-set-key (kbd "C-c v") 'magit-status)

;; wakatime-mode
(global-wakatime-mode)

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
;; smart-newline
(add-hook 'ruby-mode-hook ;; or any major-mode-hooks
  (lambda ()
  (smart-newline-mode t)))

;;
;; Multi-term
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

(custom-set-variables
 '(shell-pop-shell-type (quote ("multi-term" "*terminal<1>*" '(lambda () (multi-term)))))
 '(shell-pop-set-internal-mode "multi-term")
 '(shell-pop-term-shell "/bin/zsh")
 '(shell-pop-universal-key "<f8>")
 '(shell-pop-window-height 30)
 '(shell-pop-window-position "bottom"))


;;
;; yasnippet
;;
;;(add-to-list 'load-path
;;              "~/.emacs.d/plugins/yasnippet")
;;(require 'yasnippet)
;;(yas-global-mode 1)

;;
;; auto-insert
;;
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/insert/")
(add-to-list 'auto-insert-alist
             '("\\.rb$" . "ruby.rb")
             '("Dockerfile" . "Dockerfile"))

;;
