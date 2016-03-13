;;;;===============================================================
;;;; include lispのパッケージ管理関連
;;;;===============================================================

;;;;パッケージ管理ソフト elpaを利用 //packageのライブラリとして非公式のmelpaとmarmaladeを利用
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;ロードパスを追加(パッケージ管理lispに登録されていないものを手動で入れたlisp用のパスを追加)
(setq load-path (cons "~/.emacs.d/elpa/site-lisp/" load-path))

;;;;===============================================================
;;;; テーマ
;;;;===============================================================

(load-theme 'wheatgrass t) ;; 緑
;(load-theme 'manoj-dark t) ;;黒

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
;(define-key mode-specific-map "s" 'ansi-term)

;;; C-c w => delete whitespace
(define-key mode-specific-map "w" 'delete-trailing-whitespace)

;;;align => C-c a
(define-key mode-specific-map "a" 'align)

;;;cua-set-rectangle-mark => C-x Enter
(define-key global-map  "\C-x\C-m" 'cua-set-rectangle-mark)

;;; C-x p => helm
(define-key global-map "\C-xp" 'helm-mini)

;;; C-x y => クリップボードの中身をみる
(define-key global-map "\C-xy" 'helm-show-kill-ring)

;;; C-c y => スニペットを展開
(define-key global-map "\C-cy" 'helm-yas-complete)

;;; C-e => 選択領域に登録マクロを実行
(define-key global-map "\C-e" 'apply-macro-to-region-lines)

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

;;; ツールバーを非表示
(tool-bar-mode -1)

;;; メニューバーを非表示
(menu-bar-mode -1)

;;; 起動時にスクリーン最大化
;(set-frame-parameter nil 'fullscreen 'maximized)

;;; set color (not need after ver 22)
(global-font-lock-mode t)

;;; Ricty フォントの利用 (Macのwindows systemを用いた場合のみ)
(if (and (eq system-type 'darwin) (eq window-system 'ns))
    (progn
;; Cocoa Emacs固有の設定を書く
(create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
(set-fontset-font "fontset-ricty"
                  'unicode
                  (font-spec :family "Ricty" :size 12)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-ricty"))
))

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;; create auto-save file in ~/.emacs.d/backup
;; backupから復元したいときはM-x recover-file その後元ファイルを指定すれば良い
;(setq auto-save-file-name-transforms
;      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;;; vcを起動しないようにする(軽くするため)
(custom-set-variables
 '(vc-handled-backends nil))

;; 不要なhookを外す
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

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

;;MidightBlueに (http://life.a.la9.jp/hp/color/rgb-tab-pc.html)
(set-face-background 'hl-line "MidnightBlue")

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

;;; emacsキルリング == macクリップボード(macのみ)
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;expand region (C-uで選択範囲をいい感じに広げる)
(require 'expand-region)
(define-key global-map (kbd "C-u") nil)
(global-set-key (kbd "C-u") 'er/expand-region)

;;;undo-tree C-x uで履歴を視覚的に
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-x u") 'undo-tree-redo)

;;;バッファを閉じてもundoできるようにする
(require 'undohist)
(undohist-initialize)

;;; 行末のスペースを保存時に削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; 行末に必ず1行挿入する
(setq require-final-newline t)

;;; 自動的にバッファをリロード
(global-auto-revert-mode t)

;;;対応する括弧を虹色にする
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;対応する括弧の色を美しく調整する
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;;;検索の一致した数を表示
;(global-anzu-mode t)

;;;;===============================================================
;;;; set function
;;;;===============================================================

;;;タブを利用する(-nw enable)
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode t)

;; 例: 全バッファを一つのグループにしまう
(setq tabbar-buffer-groups-function (lambda () (list "Buffers")))
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                   (cond
                    ;; Always include ;TODO: he current buffer.
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

;;;ファイルを開くときにdired-modeを起動
(ffap-bindings)

;;;同じファイル名はディレクトリまで表示する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; M-qrr 高機能な置換モード

(defalias 'qrr 'query-replace-regexp)

;;;ディレクトリを開きrを押すとファイル名を編集できる)
(require 'dired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;最近開いたファイルを開く
(require 'recentf-ext)
(setq recentf-max-saved-items 100) ; 100個まで履歴として保存
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;;; dmacro.el の設定  キー操作の繰り返しを C-t で
(defconst *dmacro-key* "\C-t" "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;;; 一時的なメモ帳を開く C-c j
(setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
(global-set-key (kbd "C-c j") 'open-junk-file)

;;;;===============================================================
;;;; helm(anythingの後継機)
;;;;===============================================================

(require 'helm-config)
(helm-mode t)

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

;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;;;gitで管理中のファイルを開く C-x f
(require 'helm-ls-git)
(define-key global-map (kbd "C-x f") nil)
(global-set-key (kbd "C-x f") 'helm-ls-git-ls)


;;; 共通するシンボルはハイライト C-x C-aで単語をまとめて編集するモードへ
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c++-mode-common-hook 'hs-minor-mode)

;;; スニペット(コード補完)
(require 'yasnippet)
(yas-global-mode t)


;;; ポップイン
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)


;;; ウィンドウの上部に現在の関数名を表示
(which-function-mode t)

;;;googleのc/c++のルールに順守
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
;(require 'flymake)

;; c/c++
;(add-hook 'c-mode-common-hook (lambda () (flymake-mode t)))
;;もし利用したいなら以下をMakefileに書いておく必要がある
;;PHONY: check-syntax
;;check-syntax:
;;    $(CXX) -Wall -Wextra -pedantic -fsyntax-only $(CHK_SOURCES)

;;;flychekc(シンタックスエラーをリアルタイムで検出)
(add-hook 'after-init-hook #'global-flycheck-mode)

(when (require 'flycheck nil 'noerror)
  (custom-set-variables
   ;; エラーをポップアップで表示
   '(flycheck-display-errors-function
     (lambda (errors)
       (let ((messages (mapcar #'flycheck-error-message errors)))
         (popup-tip (mapconcat 'identity messages "\n")))))
   '(flycheck-display-errors-delay 0.5))
  (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
  (add-hook 'c-mode-common-hook 'flycheck-mode))

;;; make するときのオプションをしてい
(setq compile-command "make ")

;;;括弧を自動補完
(require 'smartparens-config)
(smartparens-global-mode t)

;括弧を使った時に自動で改行
;(setq c-auto-newline t)

;;;空白を一発削除
(setq c-hungry-delete-key t)

;;;shellをその場に出す
(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
;; (setq shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
;; (setq shell-pop-shell-type '("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
;;(setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
(global-set-key (kbd "C-c s") 'shell-pop)

;;;eshellにターミナルの環境変数を読み込む
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
