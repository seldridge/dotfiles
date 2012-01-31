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
(add-to-list 'load-path "~/dotfiles/.elisp/color-theme-6.6.0")
(add-to-list 'load-path "~/dotfiles/.elisp/color-theme-6.6.0/color-theme-tangotango")
(require 'color-theme-tangotango)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-tangotango)))
;;(load "~/.elisp/icicles-install")

;;-------------------------------------- Flags
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style (quote parenthesis))
(auto-compression-mode 1)
(setq debug-on-error t)
;(setq explicit-shell-file-name t)
(setq truncate-partial-width-windows nil)
;(setq explicit-shell-args '("--login" "-i"))
(setq inhibit-splash-screen t)
;;(set-background-color "black")
;;(set-foreground-color "white")
;;(set-cursor-color "white")
;(setq-default indent-tabs-mode nil)
;(setq-default tab-width 2)
;(setq tramp-default-user "root")
(setq fill-column 80)
(setq column-number-mode t)

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
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; annoyance net
(defun ask-before-inconify ()
  "Ask if you really meant to hit inconify emacs"
  (interactive)
  (if (y-or-n-p (format "Do you really want to inconify? "))
      (iconify-or-deiconify-frame)
    (message "I thought so...")))
(when window-system
  (global-set-key (kbd "C-z") 'ask-before-inconify))

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

;; run cygwin shell
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

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

(defun revert-buffer-fast ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
;;-------------------------------------- Binds
(global-set-key (kbd "M-g g") `goto-line)
(global-set-key (kbd "C-S-h") `windmove-left)
(global-set-key (kbd "C-S-j") `windmove-down)
(global-set-key (kbd "C-S-k") `windmove-up)
(global-set-key (kbd "C-S-l") `windmove-right)
;; Meta S Key Bindings 
(global-set-key (kbd "M-s t") `toggle-truncate-lines)
(global-set-key (kbd "M-s n") `shell-new)


;;-------------------------------------- Modes

;; syntax highlighting
(global-font-lock-mode 1)
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
;; verilog mode customization
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))

;; icicles
(add-to-list 'load-path "~/dotfiles/.elisp/icicles")
(require 'ring+)
(require 'icicles)

;; mode by file extension
(setq auto-mode-alist (cons '("\\.h$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.v$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.veo$". verilog-mode) auto-mode-alist))

;;-------------------------------------- Enabled Commands

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;-------------------------------------- Startup
(defun se-startup()
  ;; Creates a four panel emacs workspace and starts icicles
  (server-start)
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 1)
  (other-window 1)
  (split-window-vertically)
  (other-window 1)
  (other-window 1)
  (icy-mode 1))

(add-hook `emacs-startup-hook `se-startup)

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