;; (Theme) ;;
; (load-theme 'solarized-dark t)
(load-theme 'dichromacy t)


;; Font
;; from http://sakito.jp/emacs/emacs23.html
(when (display-graphic-p)
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
