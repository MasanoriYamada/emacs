;;;; include lispのパッケージ管理関連
;;;auto-installというパッケージ管理ソフトを使う。 パッケージの本体は以下のパス下で管理
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
;;更新を確認に行くのでココをコメントアウトしないと起動に時間がかかる
;(auto-install-update-emacswiki-package-name t)
;;install-elisp.el互換モードにする
(auto-install-compatibility-setup)
;;ediff関連のバッファを１つのフレームにまとめる
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; global key bind
;;-> =>c-l
;;(define-key global-map (kbd "C-l") nil)
;;(define-key global-map (kbd "C-l") 'forward-char)
;;<- =>c-k
;;(define-key global-map (kbd"C-k") nil)
;;(define-key global-map (kbd"C-k") 'backward-char)
;;-> =>c-;
(define-key global-map (kbd "C-;") nil)
(define-key global-map (kbd "C-;") 'forward-word)
;;<- =>c-j
(define-key global-map (kbd"C-j") nil)
(define-key global-map (kbd"C-j") 'backward-word)
;;; C-h => backword
(define-key global-map (kbd "C-h") nil)
(define-key global-map (kbd "C-h") 'delete-backward-char)
;;kill =>c-f
;;(define-key global-map (kbd"C-f") nil)
;;(define-key global-map (kbd"C-f") 'kill-line)
;;kill =>c-f
(define-key global-map (kbd"M-z") 'undo)
;;goto-line => c-x l
(define-key global-map "\C-xl" 'goto-line)
;; M-p =>anything
(define-key global-map(kbd "M-p") 'anything)
;; C-c c => compile
(define-key mode-specific-map "c" 'compile)
;; C-c d => debug mode
(define-key mode-specific-map "d" 'gdb)
;; C-c s => shell
(define-key mode-specific-map "s" 'shell)
;; C-c w => delete whitespace
(define-key mode-specific-map "w" 'delete-trailing-whitespace)
;;align => C-c a
(define-key mode-specific-map "a" 'align)
;;cua-set-rectangle-mark => C-x Enter
(define-key global-map  "\C-x\C-m" 'cua-set-rectangle-mark)
;; M-p => anything
(define-key global-map "\M-p" 'anything)
;;; for another machine
;; key bind change C-d => backword (defalt back space is binding C-d in bblqcd )
(global-set-key "\C-d" 'delete-backward-char)
;;comannd => meta at mac
(setq mac-command-modifier 'meta)

;;;command => Meta key at mac
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))


;;;; alias
;; C-M-% =>M-qrr
;(defalias 'qrr 'anything-query-replace-regexp)
(defalias 'qrr 'query-replace-regexp)

;;;; local key bind
;; c and c++-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-ca" 'align-current)
	     (define-key c-mode-map "\C-d" 'delete-backward-char)
             ))
;;;when dired mode , you can change filea name use "r"(ディレクトリを開きrを押すとファイル名を編集できる)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)



;;;; set emacs defalt
;;;set defalt of  *UTF-8 LF*。
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;;; 起動時の画面はいらない
(setq inhibit-startup-message t)
;;; set color (not need after ver 22)
(global-font-lock-mode t)
;;;not make back up file ~ #
(setq make-backup-files nil)
;; create auto-save file in ~/.emacs.d/backup
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))
;;; Always end a file with a newline(important for scripts)
(setq require-final-newline t)
;; set length of tab is 4
(setq tab-width 4)
;; delete sound and flash
(setq ring-bell-function 'ignore)
;; use space instead of tab
(setq indent-tabs-mode nil)
;; show code of new line
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
;;; c-x c-enter => rectangle select
(cua-mode t)
(setq cua-enable-cua-keys nil)
;;; add *.h is opened by c++ mode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;;; high light parenthesis
(show-paren-mode t)
;;for long parenthesis
(setq show-paren-style 'mixed)
;;; hight light current line
(global-hl-line-mode)
;;; add chomd excute at #! (sqript)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;;;全角空白とタブに色を付ける
        (defface my-face-b-1 '((t (:background "gray"))) nil)
        (defface my-face-b-2 '((t (:background "linen"))) nil)
        (defvar my-face-b-1 'my-face-b-1)
        (defvar my-face-b-2 'my-face-b-2)
        (defadvice font-lock-mode (before my-font-lock-mode ())
          (font-lock-add-keywords
           major-mode
           '(("　" 0 my-face-b-1 append)
             ("\t" 0 my-face-b-2 append)
             )))
        (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
        (ad-activate 'font-lock-mode)
;;;emacs を起動したら一番上のwindowに来るように設定
(if (eq window-system 'ns)
(x-focus-frame nil))
;;;flymake(シンタックスエラーをリアルタイムで検出)
;;c/c++
(add-hook 'c-mode-common-hook (lambda () (flymake-mode t)))
;もし利用したいなら以下をMakefileに書いておく必要がある
;PHONY: check-syntax
;check-syntax:
;    $(CXX) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)
;;;タブを利用する
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode 1)
;; 例: 全バッファを一つのグループにしまう
(setq tabbar-buffer-groups-function (lambda () (list "Buffers")))
;;
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
		          ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
			       ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
;;タブの移動のキーバインドの設定
(global-set-key (kbd "C-c n") 'tabbar-forward-tab)
(global-set-key (kbd "C-c p") 'tabbar-backward-tab)
(global-set-key (kbd "C-c k") 'kill-buffer)
;;;anything(全てをすべるもの)
(require 'anything-startup)
;;; auto-complete (should install from http://cx4a.org/software/auto-complete/index.ja.html)
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
;; add dictionary
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)
;; save history for auto-complete
(setq ac-comphist-file "~/.emacs.d/auto-complete/history/ac-comphist.dat")
;;set font color in auto-complete
(set-face-background 'ac-candidate-face "blue1")
(set-face-background 'ac-completion-face "blue1")
(set-face-background 'ac-selection-face "BlueViolet")
(set-face-underline 'ac-selection-face "white")
(set-face-foreground 'ac-candidate-face "white")
;;; migemo ローマ字のまま日本語をインクリメンタルサーチする
(require 'migemo')
;;;align (defalat)
(require 'align)
(add-to-list 'align-rules-list
             '(c++-assignment
               (regexp . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
               (justify .t)
               (tab-stop . nil)
               (modes . '(c++-mode))))
(add-to-list 'align-dq-string-modes 'c++-mode)
(add-to-list 'align-sq-string-modes 'c++-mode)
(add-to-list 'align-open-comment-modes 'c++-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                    "\\([({}\\(/\*\\)]$\\)\\|"
                                    "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                    "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                    "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"
                                    ))
;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)
;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
