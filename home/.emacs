;;--------------------------------------
;; Schuyler Eldridge
;; emacs customization
;;--------------------------------------
;;--------------------------------------
;; TODO
;; 0) Cleanup
;; 1) Custom shell creation stuff
;; 2) Colors [DONE?]
;; 4) Add rainbow-delimiters.el?
;;--------------------------------------

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

(set-exec-path-from-shell-PATH)

(add-to-list 'load-path "~/.emacs.d/elisp")
(load-library "metals.el")
(add-to-list 'load-path "~/repos/github.com/llvm/circt/llvm/llvm/utils/emacs")
(load-library "tablegen-mode.el")
(load-library "llvm-mode.el")

;;-------------------------------------- Flags
(show-paren-mode t)
(auto-compression-mode 1)
(setq-default fill-column 80)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode -1)
(blink-cursor-mode 1)
;; (set-default-font "SF Mono-9")
;; (add-to-list 'default-frame-alist
;;              '(font . "IBMPlexMono-9"))
(setq mouse-autoselect-window nil) ;; focus follows mouse off
(setq sentence-end-double-space t) ;; sentences end with a single space
(setq show-paren-delay 0)
(setq show-paren-style (quote parenthesis))
(setq truncate-partial-width-windows nil)
(setq-default indent-tabs-mode nil)
(setq cperl-indent-level 2)
(setq-default tab-width 2)
(setq tramp-auto-save-directory "/tmp")
(setq column-number-mode t)
(setq buffer-file-coding-system 'unix) ;; DOES THIS WORK??
(setq visible-bell nil)
(setq compilation-scroll-output t)
(setq comment-column 40)
(setq blink-cursor-blinks -1)
(setq debug-on-error t)
(setq inhibit-splash-screen t)
(setq user-mail-address "schuyler.eldridge@gmail.com")
(setq user-full-name "Schuyler Eldridge")
(setq sh-basic-offset 2)
(setq sh-indentation 2)
; Hack around baud rate
(setq search-slow-speed 1199)
(setq search-slow-window-lines 8)

(setenv "PATH" (concat (getenv "PATH") ":/Users/schuylere/.nix-profile/bin/"))
(setq exec-path (append exec-path '("/Users/schuylere/.nix-profile/bin/")))


;;-------------------------------------- Require
(require 'cc-mode)

;;-------------------------------------- GNUs
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 40
                         (group 1.0))
 (vertical 1.0
                         (summary 0.25 point)
                         (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 40
                         (group 1.0))
               (summary 1.0))))

(gnus-add-configuration
 '(group
   (horizontal 1.0
               (vertical 40
                         (group 1.0))
               (vertical 1.0
                         (summary 0.25 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(reply
   (horizontal 1.0
               (vertical 40
                         (group 1.0))
               (vertical 1.0
                         (article 0.25 point)
                         (message 1.0)))))

(defun gnus-demon-scan-news-3 ()
  (let ((win (current-window-configuration))
	(gnus-read-active-file 'some)
	(gnus-check-new-newsgroups nil)
	(gnus-verbose 2)
	(gnus-verbose-backends 5)
	(level 3)
	)
    ;; (message "check-mail: %s" (format-time-string "%H:%M:%S"))
    (while-no-input
      (unwind-protect
          (save-window-excursion
            (when (gnus-alive-p)
              (with-current-buffer gnus-group-buffer
                (gnus-group-get-new-news level))))
        (set-window-configuration win)))))

(setq gnus-demon-timestep 10)
(gnus-demon-add-handler 'gnus-demon-scan-news-3 12 1)

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

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq comint-buffer-maximum-size 9999)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(add-hook 'text-mode-hook 'flyspell-mode)

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
(global-set-key (kbd "M-s c") `compile)
(global-set-key (kbd "M-s :") `comment-indent)
;;(global-set-key (kbd "M-s g") `magit-status)
;; mode specific bindings
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
;; Hippie Expand
;; (global-set-key (kbd "M-/") `hippie-expand)
(global-set-key (kbd "M-=") `count-words)
;; Company Mode
;; (global-set-key (kbd "M-/") 'company-complete)

;;-------------------------------------- Modes

;; (setq magit-auto-revert-mode nil)
;; (setq magit-last-seen-setup-instructions "1.4.0")

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
      verilog-auto-lineup              nil
;;      verilog-highlight-p1800-keywords nil
;;      verilog-linter                   "my_lint_shell_command"
      verilog-auto-delete-trailing-whitespace t
      verilog-date-scientific-format t
      verilog-company "Boston University"
        )

;;-------------------------------------- File Extensions
;; miscellaneous
(setq auto-mode-alist (cons '("\\.h$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h.inc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h++$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.c++$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hpp$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cpp$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cpp.inc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hh$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pde$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.v$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.veo$". verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cu$". cuda-mode) auto-mode-alist))
;; gem5
(setq auto-mode-alist (cons '("\\.cc\\.inc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hh\\.inc$". c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.isa$". python-mode) auto-mode-alist))
;; Github-flavored Markdown
(setq auto-mode-alist (cons '("\\.md$". gfm-mode) auto-mode-alist))
;; Makefiles
(setq auto-mode-alist (cons '("Makefrag". makefile-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.d$". makefile-mode) auto-mode-alist))
;; RISC-V related
(setq auto-mode-alist (cons '("\\.rvS". asm-mode) auto-mode-alist))
;; LaTeX
(setq auto-mode-alist (cons '("\\.tex". latex-mode) auto-mode-alist))

;; company mode
;; (setq company-idle-delay 0.5)

;;-------------------------------------- Enabled Commands

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;-------------------------------------- Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)


(require 'magit)

;;-------------------------------------- Scala

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false")))

;; ;; Enable nice rendering of diagnostics like compile errors.
;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; (use-package lsp-mode
;;   ;; Optional - enable lsp-mode automatically in scala files
;;   ;; You could also swap out lsp for lsp-deffered in order to defer loading
;;   :hook  (scala-mode . lsp)
;;          (lsp-mode . lsp-lens-mode)
;;   :config
;;   ;; Uncomment following section if you would like to tune lsp-mode performance according to
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   ;; (setq gc-cons-threshold 100000000) ;; 100mb
;;   ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   ;; (setq lsp-idle-delay 0.500)
;;   ;; (setq lsp-log-io nil)
;;   ;; (setq lsp-completion-provider :capf)
;;   (setq lsp-prefer-flymake nil)
;;   ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
;;   (setq lsp-keep-workspace-alive nil))

;; ;; Add metals backend for lsp-mode
;; (use-package lsp-metals)

;; ;; Enable nice rendering of documentation on hover
;; ;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;; ;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;; ;;   In that case you have to not only disable this but also remove from the packages since
;; ;;   lsp-mode can activate it automatically.
;; (use-package lsp-ui)

;; ;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; ;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; ;; to avoid odd behavior with snippets and indentation
;; (use-package yasnippet)

;; ;; Use company-capf as a completion provider.
;; ;;
;; ;; To Company-lsp users:
;; ;;   Company-lsp is no longer maintained and has been removed from MELPA.
;; ;;   Please migrate to company-capf.
;; (use-package company
;;   :hook (scala-mode . company-mode)
;;   :config
;;   (setq lsp-completion-provider :capf))

;; ;; Posframe is a pop-up tool that must be manually installed for dap-mode
;; (use-package posframe)

;; ;; Use the Debug Adapter Protocol for running tests and debugging
;; (use-package dap-mode
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode))

(setenv "PATH" (concat (getenv "PATH") ":~/Library/Application Support/Coursier/bin"))
(setq exec-path (append exec-path '("~/Library/Application Support/Coursier/bin")))

;;-------------------------------------- Theme
(load-theme 'tangotango t)

;;-------------------------------------- Hooks
(defun se-startup()
  ;; Creates a four panel emacs workspace
  (server-start)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
)

(defun latex-startup()
  (define-key latex-mode-map "\M-q" 'maybe-fill-paragraph)
  ;; (define-key latex-mode-map "\M-q" 'fill-paragraph)
  (visual-line-mode))

;; Currently not working...
(defun gfm-startup()
  (local-set-key "\M-q" 'fill-paragraph)
  (visual-line-mode))

;; Scala mode setup
;; (defun scala-mode-setup()
;;   (flyspell-prog-mode)
;;   (set-fill-column 120))

;;-------------------------------------- Setup clang-format
(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

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

;;-------------------------------------- Spell check
(setq-default ispell-program-name "ispell")

;;-------------------------------------- Hooks
(add-hook `emacs-startup-hook `se-startup)
(add-hook `before-save-hook `delete-trailing-whitespace)
;; (remove-hook `before-save-hook `delete-trailing-whitespace)
(add-hook `latex-mode-hook `latex-startup)
(add-hook `markdown-mode-hook `gfm-startup)
(add-hook `gfm-mode-hook `gfm-startup)
(add-hook `python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook `scala-mode-hook `scala-mode-setup)
;; (add-hook 'after-init-hook 'global-company-mode)

(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;;---------------------------------------- MLIR
(add-to-list 'load-path "~/repos/github.com/llvm/circt/llvm/mlir/utils/emacs")
(load-library "mlir-mode.el")
(load-library "mlir-lsp-client.el")
(lsp-mlir-setup)
(setq lsp-mlir-server-executable "~/repos/github.com/llvm/circt/build/bin/circt-lsp-server")

;;---------------------------------------- Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(any-variable EXPR)
 '(case-fold-search t)
 '(custom-safe-themes
   '("713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" "4e63466756c7dbd78b49ce86f5f0954b92bf70b30c01c494b37c586639fa3f6f" default))
 '(git-commit-summary-max-length 50)
 '(package-selected-packages
   '(lsp-mode mlir-lsp-client lsp-sourcekit racket-mode lsp-ui tree-sitter swift-mode cmake-mode xml-format dockerfile-mode ess flycheck-clang-tidy flycheck-aspell clang-format lsp-metals go-mode csv-mode edit-server graphviz-dot-mode flycheck use-package yaml-mode mutt-mode google-c-style haskell-mode markdown-mode tangotango-theme firrtl-mode magit json-mode scala-mode2 polymode gmail-message-mode bbdb))
 '(safe-local-variable-values
   '((eval end-of-buffer)
     (eval c-add-style "m5"
           '((c-basic-offset . 4)
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
              (innamespace . 0))))))
 '(scala-indent:align-forms t)
 '(scala-indent:align-parameters t)
 '(scala-indent:default-run-on-strategy 1)
 '(tags-case-fold-search nil)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tex-verbatim ((t (:family "inconsolata")))))
