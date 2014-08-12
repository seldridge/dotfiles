;;--------------------------------------
;; Schuyler Eldridge
;; emacs customization
;;--------------------------------------
;;--------------------------------------
;; TODO
;; 0) Cleanup
;; 1) Custom shell creation stuff
;; 2) Colors [DONE?]
;; 3) Fix how prepend-path is handled - what?
;; 4) Add rainbow-delimiters.el?
;;--------------------------------------

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(setq debug-on-error t)
(setq inhibit-splash-screen t)

;; generic path defuns - what do these do exactly?
(defun prepend-path ( my-path )
  (setq load-path (cons (expand-file-name my-path) load-path)))
(defun append-path ( my-path )
  (setq load-path (append load-path (list (expand-file-name my-path)))))
;; Look first in the directory ~/.elisp for elisp files
(prepend-path "~/dotfiles/.elisp/")
;; load matlab mode
(add-to-list 'load-path "~/dotfiles/.elisp/matlab-emacs/matlab-emacs")
(load-library "matlab-load")
;; color-theme stuff - remove this when emacs 24 is available
(add-to-list 'custom-theme-load-path "~/dotfiles/themes")
(load-theme 'tangotango t)
;; (add-to-list 'load-path "~/dotfiles/.elisp/color-theme-6.6.0")
;; (add-to-list 'load-path "~/dotfiles/.elisp/color-theme-6.6.0/color-theme-tangotango")
(require 'cc-mode)
;; (require 'color-theme-tangotango)
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-tangotango)))
;;(load "~/.elisp/icicles-install")
;;(add-to-list 'load-path "~/dotfiles/.elisp/magit-1.1.1")
;;(require `magit)
(require `edit-server)
(edit-server-start)

;;-------------------------------------- Flags
(set-default-font "Inconsolata-11")
(setq mouse-autoselect-window nil) ;; focus follows mouse off
(setq sentence-end-double-space nil) ;; sentences end with a single space
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style (quote parenthesis))
(auto-compression-mode 1)
;(setq explicit-shell-file-name t)
;(setq explicit-shell-args '("--login" "-i"))
(setq truncate-partial-width-windows nil)
;;(set-background-color "black")
;;(set-foreground-color "white")
;;(set-cursor-color "white")
(setq-default indent-tabs-mode nil)
(setq cperl-indent-level 2)
;(setq-default tab-width 2)
;(setq tramp-default-user "root")
(setq tramp-auto-save-directory "/tmp")
(set-fill-column 80)
(setq column-number-mode t)
(setq buffer-file-coding-system 'unix) ;; DOES THIS WORK??
(setq visible-bell t)
(setq compilation-scroll-output t)
(set-default-font "Inconsolata 11")
(setq comment-column 40)

;;-------------------------------------- Functions

;; safety net
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
;;      (if (< emacs-major-version 22)  ;; this doesn't seem to work...
;;          (save-buffers-kill-terminal)
      (save-buffers-kill-emacs);;)
    (message "Whew... you owe me...")))

;; annoyance net
(defun ask-before-inconify ()
  "Ask if you really meant to hit inconify emacs"
  (interactive)
  (if (y-or-n-p (format "Do you really want to inconify? "))
      (iconify-or-deiconify-frame)
    (message "I thought so...")))

;; create new shell
(defun shell-new ()
  "Creates new shell from user string."
  (interactive)
  (setq buffer
        (read-string "Enter shell name: "))
  (shell (concat "$" buffer)))
;;(shell "*shell*<%s>" `shell-name))

(setq comint-buffer-maximum-size 9999)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(add-hook 'text-mode-hook 'flyspell-mode)

;; run cygwin shell
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))
;; Attempts to get cygwin ssh to work through emacs shell
;;(require 'tramp)
;;(setq tramp-default-method "ssh")
;;(nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods)))
;;       '(("bash" "-i")))
;;(setcdr (assq 'tramp-remote-sh (assoc "ssh" tramp-methods))
;;      '("bash -i"))

(defun my-filter (condp lst)
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart
  way.  If a buffer with a running shell process exists, simply
  switch to that buffer.  If a shell buffer exists, but the shell
  process is not running, restart the shell.  If already in an
  active shell buffer, switch to the next one, if any.  With
  prefix argument CREATE always start a new shell."
  (interactive "P")
  (let ((next-shell-buffer) (buffer)
        (shell-buf-list (identity ;;used to be reverse
                         (sort
                          (my-filter (lambda (x) (string-match "^\\*shell\\*" (buffer-name x))) (buffer-list))
                          '(lambda (a b) (string< (buffer-name a) (buffer-name b)))))))
    (setq next-shell-buffer
          (if (string-match "^\\*shell\\*" (buffer-name buffer))
              (get-buffer (cadr (member (buffer-name) (mapcar (function buffer-name) (append shell-buf-list shell-buf-list)))))
            nil))
    (setq buffer
          (if create
              (generate-new-buffer-name "*shell*")
            next-shell-buffer))
    (shell buffer)))
;; count words in the region
(defun count-words-region (start end)
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      (message "Region has %d words" n)
      n)))

(defun count-region (beginning end)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting...")
  (save-excursion
    (let (word_count char_count)
      (setq word_count 0)
      (setq char_count (- end beginning))
      (goto-char beginning)
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq word_count (1+ word_count)))
      (message "Words: %d | Characters: %d" word_count char_count)
)))

(defun revert-buffer-fast ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
;;-------------------------------------- Binds
(global-set-key (kbd "M-g g") `goto-line)
(global-set-key (kbd "C-S-h") `windmove-left)
(global-set-key (kbd "C-S-j") `windmove-down)
(global-set-key (kbd "C-S-k") `windmove-up)
(global-set-key (kbd "C-S-l") `windmove-right)
(global-set-key (kbd "<C-tab>") `other-frame)
(global-set-key (kbd "C-x C-b") `buffer-menu)
(when window-system
  (global-set-key (kbd "C-z") 'ask-before-inconify))
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))
;; meta s key bindings
(global-set-key (kbd "M-s t") `toggle-truncate-lines)
(global-set-key (kbd "M-s n") `shell-new)
(global-set-key (kbd "M-s r") `revert-buffer-fast)
(global-set-key (kbd "M-s ;") `comment-dwim)
(global-set-key (kbd "M-s c") `compile)
(global-set-key (kbd "M-s :") `comment-indent)
;;(global-set-key (kbd "M-s g") `magit-status)
;; mode specific bindings
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;-------------------------------------- Modes

;; syntax highlighting
(global-font-lock-mode 1)

;; cuda-mode
(autoload 'cuda-mode "cuda-mode" "Cuda Mode" t)

;; verilog-mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
;;      verilog-indent-level-directive   1
;;      verilog-case-indent              2
     verilog-auto-newline             nil
;;      verilog-auto-indent-on-newline   t
;;      verilog-tab-always-indent        t
      verilog-auto-endcomments         nil
;;      verilog-minimum-comment-distance 40
;;      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'all
;;      verilog-highlight-p1800-keywords nil
;;      verilog-linter                   "my_lint_shell_command"
      verilog-auto-delete-trailing-whitespace t
      verilog-date-scientific-format t
      verilog-company "Boston University"
        )

;; matlab mode
;;(setq matlab-indent-level 2
;;      matlab-cont-level   2)
;;      matlab-case-level  '(1.1))
;;      matlab-auto-fill    )

;; icicles
;; (add-to-list 'load-path "~/dotfiles/.elisp/icicles")
;; (require 'ring+)
;; (require 'icicles)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

;; mode by file extension
(setq auto-mode-alist (cons '("\\.h$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h++$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c++$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cpp$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hh$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.v$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.veo$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$". cuda-mode) auto-mode-alist))

;;-------------------------------------- Enabled Commands

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;-------------------------------------- Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;;-------------------------------------- Startup
(defun se-startup()
  ;; Creates a four panel emacs workspace
  (server-start)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 1)
  (other-window 1)
  (split-window-vertically)
  (other-window 1)
  (other-window 1)
;; (icy-mode 1)
)

;;-------------------------------------- Hooks
(add-hook `emacs-startup-hook `se-startup)
(add-hook `before-save-hook `delete-trailing-whitespace)

;;-------------------------------------- Test Area
 ;; Sets your shell to use cygwin's bash, if Emacs finds it's running
  ;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
  ;; not already in your Windows Path (it generally should not be).
  ;;
  (let* ((cygwin-root "c:/cygwin")
         (cygwin-bin (concat cygwin-root "/bin")))
    (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))

      (setq exec-path (cons cygwin-bin exec-path))
      (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

      ;; By default use the Windows HOME.
      ;; Otherwise, uncomment below to set a HOME
      ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

      ;; NT-emacs assumes a Windows shell. Change to baash.
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)

      ;; This removes unsightly ^M characters that would otherwise
      ;; appear in the output of java applications.
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(any-variable EXPR)
 '(custom-safe-themes
   (quote
    ("dbf8cb30319aa88d14c569ef4509bd2c9ad6c8c58e7e7a7ae61a872cb32e9de2" "7f329ccc6b229c2172dc540848aa195dd9fbd508bc96618a1ab0b1955dd1a5a7" "40517b254c121bf4d62d1b0a61075959d721f928ed941aa6a3c33a191ebb1490" "64905e72f368db9bbc1fc347e8c3ab016257c4286006be72b9e50db72b3b5164" "e5d899e8ca6ae9014855c533993b0c06095bb7c55a5b0aab8e71a07d94d9e352" default)))
 '(safe-local-variable-values
   (quote
    ((eval c-add-style "m5"
           (quote
            ((c-basic-offset . 4)
             (indent-tabs-mode)
             (c-offsets-alist
              (substatement-open . 0)
              (inline-open . 0)
              (block-open . -4)
              (case-label . 2)
              (label . 2)
              (statement-case-intro . 2)
              (statement-case-open . 2)
              (access-label . -2)
              (innamespace . 0)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tex-verbatim ((t (:family "inconsolata")))))
