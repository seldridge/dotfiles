;;--------------------------------------
;; Schuyler Eldridge .emacs
;;--------------------------------------

;;-------------------------------------- Setup Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;-------------------------------------- Load non-package .emacs/*.el files
(setq load-path
      (cons (expand-file-name "~/.emacs.d/opt") load-path))
(require 'llvm-mode)
(require 'tablegen-mode)
(require 'mlir-mode)
(require 'wake-mode)

;;-------------------------------------- General Configuration

;; Basic display setup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode -1)
(show-paren-mode t)
(blink-cursor-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis
      blink-cursor-blinks -1
      visible-bell nil
      column-number-mode t
      inhibit-splash-screen t
      truncate-partial-width-windows nil
      mouse-autoselect-window t)
(global-font-lock-mode 1)

;; Tabs, spaces, etc.
(setq-default fill-column 80
              tab-width 2
              indent-tabs-mode nil)
(setq comment-column 40
      sentence-end-double-space t)

(setq tramp-auto-save-directory "/tmp"
      buffer-file-coding-system 'unix
      compilation-scroll-output t
; Hack around baud rate
      search-slow-speed 1199
      search-slow-window-lines 8)

;; Miscellaneous
(auto-compression-mode 1)
(setq-default ispell-program-name "ispell")
(setq debug-on-error nil
      comint-buffer-maximum-size 9999
      case-fold-search t
      custom-file "~/.emacs.d/custom.el")

;; Enabled commands (that are disabled by default)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;-------------------------------------- Theme
(require-theme 'modus-themes)
(load-theme 'modus-vivendi)

;;-------------------------------------- Functions

(defun split-window-horizontally-n(n)
  "Split a window horizontally n-times."
  (let ((i 1))
    (while (< i n)
      (split-window-horizontally)
      (setq i (+ i 1))))
  (balance-windows))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-emacs)
    (message "Whew... you owe me...")))

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

(use-package ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

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

(defun count-words-region (start end)
  "Count the number of words in a region."
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
(defun desktop-load ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(defun maybe-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').

Does nothing if `visual-line-mode' is on."
  (interactive (progn
    	 (barf-if-buffer-read-only)
    	 (list (if current-prefix-arg 'full) t)))
  (or visual-line-mode
      (fill-paragraph justify region)))

(defun se-startup()
  (server-start)
  (split-window-horizontally-n 4)
  (set-exec-path-from-shell-PATH))

;;-------------------------------------- Minor modes

;;------------------ ace-window
(use-package ace-window)

;;------------------ magit-mode
(use-package magit)
(setq git-commit-summary-max-length 50
      git-commit-fill-column 74)

;;-------------------------------------- Binds

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-=") `count-words)
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing)
  (global-set-key (kbd "C-z") 'ask-before-inconify))

;; M-s key bindings
(global-set-key (kbd "M-s t") `toggle-truncate-lines)
(global-set-key (kbd "M-s n") `shell-new)
(global-set-key (kbd "M-s r") `revert-buffer-fast)
(global-set-key (kbd "M-s c") `compile)
(global-set-key (kbd "M-s :") `comment-indent)

;;-------------------------------------- Hooks (miscellaneous)

(add-hook `emacs-startup-hook `se-startup)
(add-hook `before-save-hook `delete-trailing-whitespace)
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;;-------------------------------------- Major Modes

;;------------------ asm-mode
(setq auto-mode-alist (cons '("\\.rvS". asm-mode) auto-mode-alist))

;;------------------ cc-mode
(use-package cc-mode)
(use-package clang-format)

(setq clang-format-style-option "llvm")

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
;; (helm-mode)
;; (require 'helm-xref)
;; (define-key global-map [remap find-file] #'helm-find-files)
;; (define-key global-map [remap execute-extended-command] #'helm-M-x)
;; (define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c u") 'clang-format-buffer)
            (local-set-key (kbd "C-c i") 'clang-format-region)))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
;;      company-idle-delay 0.0
;;      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(setq auto-mode-alist (cons '("\\.h$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.h.inc$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.c$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.h++$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.c++$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.hpp$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.cpp$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.cpp.inc$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.hh$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.cc$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.pde$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.cc\\.inc$". c++-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.hh\\.inc$". c++-mode) auto-mode-alist))

;;------------------ firrtl-mode
(use-package firrtl-mode)

;;------------------ groovy-mode
(use-package groovy-mode)
(setq groovy-indent-offset 2
      auto-mode-alist (cons '("\\.Jenkinsfile". groovy-mode) auto-mode-alist))

;;------------------ latex-mode
(setq auto-mode-alist (cons '("\\.tex". latex-mode) auto-mode-alist))

(defun latex-startup()
  (define-key latex-mode-map "\M-q" 'maybe-fill-paragraph)
  ;; (define-key latex-mode-map "\M-q" 'fill-paragraph)
  (visual-line-mode))
(add-hook `latex-mode-hook `latex-startup)

;;------------------ lua-mode
(use-package lua-mode)

;;------------------ makefile-mode
(setq auto-mode-alist (cons '("Makefrag". makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.d$". makefile-mode) auto-mode-alist))

;;------------------ markdown-mode
(use-package markdown-mode)
(setq auto-mode-alist (cons '("\\.md$". gfm-mode) auto-mode-alist))

;; Currently not working...
(defun gfm-startup()
  (local-set-key "\M-q" 'fill-paragraph)
  (visual-line-mode))
(add-hook `gfm-mode-hook `gfm-startup)
(add-hook `markdown-mode-hook `gfm-startup)

;;------------------ perl-mode/cperl-mode
(setq cperl-indent-level 2)

;;------------------ python-mode
(add-hook `python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;;------------------ scala-mode

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; (setq gc-cons-threshold 100000000) ;; 100mb
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; (setq lsp-idle-delay 0.500)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(setq scala-indent:align-forms t
      scala-indent:align-parameters t
      scala-indent:default-run-on-strategy 1)

(setq auto-mode-alist (cons '("\\.sc$". scala-mode) auto-mode-alist))

;;------------------ sh-mode
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;;------------------ text-mode
(add-hook 'text-mode-hook 'flyspell-mode)

;;------------------ verilog-mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))
(setq verilog-indent-level             2)
(setq verilog-indent-level-module      2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-behavioral  2)
(setq verilog-auto-newline             nil)
(setq verilog-auto-endcomments         nil)
(setq verilog-auto-lineup              nil)
(setq verilog-auto-delete-trailing-whitespace t)
(setq verilog-date-scientific-format t)

(setq auto-mode-alist (cons '("\\.v$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.veo$". verilog-mode) auto-mode-alist))
