;;;; global key bind
;;goto-line => c-x l
(define-key global-map "\C-xl" 'goto-line)
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
;;; key bind change C-d => backword (defalt back space is binding C-d in bblqcd )
(global-set-key "\C-d" 'delete-backward-char)

;;;; local key bind
;; c and c++-mode
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-ca" 'align-current)
	     (define-key c-mode-map "\C-d" 'delete-backward-char)
             ))



;;;; set emacs defalt
;;;set defalt of  *UTF-8 LF*。
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
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


;;;; include lisp
;;;auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
;(require 'auto-install)
;(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)

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

