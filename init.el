;;;;===============================================================
;;;; include lispのパッケージ管理関連
;;;;===============================================================

;;;;パッケージ管理ソフト elpaを利用 //packageのライブラリとして非公式のmelpaとmarmaladeを利用
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;===============================================================
;;;; テーマ
;;;;===============================================================

;(load-theme 'manoj-dark t)

;;;;===============================================================
;;;; global key bind
;;;;===============================================================

;;;-> =>c-;
(define-key global-map (kbd "C-;") nil)
(define-key global-map (kbd "C-;") 'forward-word)

;;;<- =>c-j
(define-key global-map (kbd"C-j") nil)
(define-key global-map (kbd"C-j") 'backward-word)

;;; C-h => backword
(define-key global-map (kbd "C-h") nil)
(keyboard-translate ?\C-h ?\C-?)

;;;goto-line => c-x l
(define-key global-map "\C-xl" 'goto-line)

;;; C-c c => compile
(define-key mode-specific-map "c" 'compile)

;;; C-c d => gdb
(define-key mode-specific-map "d" 'gdb)

;;; C-c s => shell
(define-key mode-specific-map "s" 'ansi-term)

;;; C-c w => delete whitespace
(define-key mode-specific-map "w" 'delete-trailing-whitespace)

;;;align => C-c a
(define-key mode-specific-map "a" 'align)

;;;cua-set-rectangle-mark => C-x Enter
(define-key global-map  "\C-x\C-m" 'cua-set-rectangle-mark)

;;; C-x p => helm
(define-key global-map "\C-xp" 'helm-mini)

;;;comannd => meta at mac
(setq mac-command-modifier 'meta)

;;;;===============================================================
;;;; local key bind
;;;;===============================================================

;;; c++
(add-hook 'c++-mode-user-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-ca" 'align-current)
             ))
;;; c
(add-hook 'c-mode-user-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-ca" 'align-current)
             ))

;;;;===============================================================
;;;; set emacs defalt
;;;;===============================================================

;;;set defalt of  *UTF-8 LF*。
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;;; 起動時の画面はいらない
(setq inhibit-startup-message t)

;;; set color (not need after ver 22)
(global-font-lock-mode t)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;; create auto-save file in ~/.emacs.d/backup
;; backupから復元したいときはM-x recover-file その後元ファイルを指定すれば良い
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;;; set length of tab is 4
(setq tab-width 4)

;;; delete sound and flash
(setq ring-bell-function 'ignore)

;;; use space instead of tab
(setq indent-tabs-mode nil)

;;; sound off
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;; show code of new line
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

;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)

;;; for long parenthesis
(setq show-paren-style 'mixed)

;;; hight light current line
(global-hl-line-mode)

;;; add chomd excute at #! (sqript)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;;; 全角空白とタブに色を付ける
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

;;; emacs を起動したら一番上のwindowに来るように設定
(if (eq window-system 'ns)
(x-focus-frame nil))

;;; キルリング=クリップボード化 (unavailable -nw)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\C-y" 'clipboard-yank)

;;;expand region (C-uで選択範囲をいい感じに広げる)
(require 'expand-region)
(define-key global-map (kbd "C-u") nil)
(global-set-key (kbd "C-u") 'er/expand-region)

;;;undo-tree C-x uで履歴を視覚的に
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-x u") 'undo-tree-redo)

;;; 行末のスペースを保存時に削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 行末に必ず1行挿入する
(setq require-final-newline t)

;;; 自動的にバッファをリロード
(global-auto-revert-mode 1)

;;;;===============================================================
;;;; set function
;;;;===============================================================

;;;タブを利用する(-nw enable)
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode 1)

;; 例: 全バッファを一つのグループにしまう
(setq tabbar-buffer-groups-function (lambda () (list "Buffers")))
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

;;;マークダウンモードを追加
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

;; .mdのファイルをマークダウンモードで開く
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;ファイルを開くときにdired-modeを起動
(ffap-bindings)

;;; M-qrr 高機能な置換モード

(defalias 'qrr 'query-replace-regexp)

;;;ディレクトリを開きrを押すとファイル名を編集できる)
(require 'dired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;最近開いたファイルを開く
(require 'recentf-ext)
(setq recentf-max-saved-items 100) ; 100個まで履歴として保存
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;;;;===============================================================
;;;; helm(anythingの後継機)
;;;;===============================================================

(require 'helm-config)
(helm-mode 1)

;;;;======================================================================
;;;; magit(git from emacs)
;;;;======================================================================

(require 'magit)

;; C-c gでmagit起動
(define-key global-map (kbd "C-c g") 'magit-status)

(require 'git-gutter)
(global-git-gutter-mode t)

;;;;======================================================================
;;;; auto-complete (自動補完)
;;;;======================================================================

(require 'auto-complete-config)
(global-auto-complete-mode t);yasnippetと共存するために必要

;;;set font color in auto-complete
(set-face-background 'ac-candidate-face "blue1")
(set-face-background 'ac-completion-face "blue1")
(set-face-background 'ac-selection-face "BlueViolet")
(set-face-underline 'ac-selection-face "white")
(set-face-foreground 'ac-candidate-face "white")

;;;;===============================================================
;;;; for programing
;;;;===============================================================

;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)
;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;;; 共通するシンボルはハイライト C-x C-aで単語をまとめて編集するモードへ
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c++-mode-common-hook 'hs-minor-mode)

;;; スニペット(コード補完)
(require 'yasnippet)
(yas-global-mode 1)


;;; ポップイン
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)


;;; ウィンドウの上部に現在の関数名を表示
(which-function-mode 1)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++mode-common-hook 'google-set-c-style)

;;; align (C-c aでコードの整形を行う)
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


;;; flymake(シンタックスエラーをリアルタイムで検出)

;; c/c++
(add-hook 'c-mode-common-hook (lambda () (flymake-mode t)))
;もし利用したいなら以下をMakefileに書いておく必要がある
;PHONY: check-syntax
;check-syntax:
;    $(CXX) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)

;;; make するときのオプションをしてい
(setq compile-command "make ")

;括弧を使った時に自動で改行
(setq c-auto-newline t)

;;;空白を一発削除
(setq c-hungry-delete-key t)
