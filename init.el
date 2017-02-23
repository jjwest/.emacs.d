; init.el --- My Emacs config
;;; Commentary:
;;; Nothing here
;;; Code:

;; Better garbage collection settings
(setq gc-cons-threshold (* 20 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Benchmark startup time
(use-package benchmark-init
  :ensure t
  :config (benchmark-init/activate))

(use-package general
  :ensure t
  :defines my-leader
  :config
  (setq general-default-keymaps 'normal)
  (setq my-leader ",")
  (general-define-key :prefix my-leader
		      :keymaps '(normal visual)
		      "n" 'narrow-or-widen-dwim)
  (general-define-key :prefix my-leader
   "dw" 'delete-window
   "do" 'delete-other-windows
   "sf" 'save-buffer
   "sa" 'my/save-all-buffers
   "k" 'kill-this-buffer
   "B" 'ibuffer
   "P" 'proced
   "W" 'winner-undo
   "ss" 'my/split-window-horizontal
   "vv" 'my/split-window-vertical))

;; General settings and better defaults
(setq initial-major-mode 'fundamental-mode
      custom-safe-themes t
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      inhibit-startup-screen t
      initial-scratch-message ""
      load-prefer-newer t
      indent-tabs-mode nil
      ad-redefinition-action 'accept
      uniquify-buffer-name-style 'forward
      message-log-max 200
      bidi-paragraph-direction 'left-to-right
      require-final-newline t
      auto-revert-check-vc-info t
      show-paren-delay 0
      save-interprogram-paste-before-kill t
      select-enable-clipboard t
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil
      locale-coding-system 'utf-8)

;; I like my backups hidden and in abundance
(unless (file-exists-p "~/.emacs.d/backups")
  (mkdir "~/.emacs.d/backups/per-save" t)
  (mkdir "~/.emacs.d/backups/per-session" t))

 (setq backup-directory-alist '(("" . "~/.emacs.d/backups/per-save"))
       backup-by-copying t
       delete-old-versions t
       kept-new-versions 10
       kept-old-versions 0
       auto-save-default nil
       vc-make-backup-files t
       version-control t)

(defun force-backup-of-buffer ()
  "Always save file backups on save."
  (unless buffer-backed-up
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backups/per-session"))))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)


;; Misc
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq-default cursor-in-non-selected-windows nil
	      fill-column 80)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't litter my init file
(unless (file-exists-p "~/.emacs.d/local")
  (mkdir "~/.emacs.d/local"))
(setq custom-file "~/.emacs.d/local/custom-set.el")

;; Set font
(when (member "Office Code Pro" (font-family-list))
  (if (equal (display-pixel-height) 1440)
      (set-face-attribute 'default nil
			  :family "Office Code Pro"
			  :foundry 'ADBO
			  :slant 'normal
			  :weight 'normal
			  :height 113
			  :width 'normal)
    (set-face-attribute 'default nil
			:family "Office Code Pro"
			:foundry 'ADBO
			:slant 'normal
			:weight 'normal
			:height 98
			:width 'normal) ))


;; Strip UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Noice utility modes
(blink-cursor-mode 0)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode t)
(winner-mode 1)
(save-place-mode 1)
(display-time)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'subword-mode)

;; Changing active window
(general-define-key :keymaps 'global
 "C-h" #'windmove-left
 "C-j" #'windmove-down
 "C-k" #'windmove-up
 "C-l" #'windmove-right
 "M-%" #'async-shell-command)



;; My own convenience functions
(defun my/split-line ()
  "Split line at point."
  (interactive)
  (save-excursion
    (newline-and-indent)))

(defun my/split-window-horizontal ()
  "Split window horizontally and change to new window."
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(defun my/split-window-vertical ()
  "Split window vertically and change to new window."
  (interactive)
  (split-window-vertically)
  (windmove-down))

(defun my/dont-kill-scratch ()
  "When scratch buffer is killed, bury instead."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (bury-buffer)
    nil))

(add-hook 'kill-buffer-query-functions #'my/dont-kill-scratch)

(defun find-file-sudo ()
  "Reopen the current file as root, preserving point position."
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
    (goto-char p)))

(defun my/save-all-buffers ()
  "Save all open buffers without prompt."
  (interactive)
  (save-some-buffers t))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one)
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  (custom-theme-set-faces
   'doom-one
   `(nlinum-relative-current-face ((t (:foreground "#46D9FF" :bold t)))))
  (with-eval-after-load 'multi-term
    (set-face-attribute 'term-color-blue nil :foreground "#51afef")
    (set-face-attribute 'term-color-green nil :foreground "#98be65")
    (set-face-attribute 'term-color-red nil :foreground "#ff6c6b")
    (set-face-attribute 'term-color-magenta nil :foreground "#c678dd")
    (set-face-attribute 'term-color-cyan nil :foreground "#46D9FF")
    (set-face-attribute 'term-color-yellow nil :foreground "#ECBE7B")))

(use-package doom-modeline
  :ensure powerline
  :ensure s
  :ensure f
  :load-path "~/.emacs.d/themes"
  :config
  (unless (file-exists-p "~/.emacs.d/themes/doom-modeline.elc")
    (add-hook 'after-init-hook
	      (lambda ()
		(byte-compile-file "~/.emacs.d/themes/doom-modeline.el")))))

(use-package evil
  :ensure t
  :config
  (general-define-key
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "TAB" 'indent-for-tab-command
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right
   "M-k" 'evil-scroll-up
   "M-j" 'evil-scroll-down
   "C-a" 'beginning-of-line
   "C-q" 'evil-scroll-line-up
   "C-e" 'evil-scroll-line-down
   "S" 'my/split-line
   "U" 'redo
   "M-." 'xref-find-definitions
   "M-," 'xref-find-references
   "Q" "@q"
   "Y" "y$")
  (general-define-key :keymaps 'evil-visual-state-map
  		      "TAB" 'indent-for-tab-command)
  (general-define-key :keymaps 'package-menu-mode-map
		      "j" 'evil-next-visual-line
		      "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

(use-package evil-visualstar
    :ensure t
    :config (global-evil-visualstar-mode))

(use-package evil-mc
  :ensure t
  :config
  (general-define-key :keymaps 'evil-mc-key-map
		      :states 'normal
		      "C-p" nil)
  (global-evil-mc-mode))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'html-mode-hook #'yas-minor-mode)
  :config
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun my/company-abort-and-newline ()
    (interactive)
    (company-abort)
    (newline-and-indent))
  :bind (("C-." . company-complete)
	 :map company-active-map
	 ("<C-return>" . my/company-abort-and-newline)
	 ("<tab>" . nil))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (general-define-key "C-." 'company-complete)
  (general-define-key :states '(normal insert)
		      "C-." 'company-complete)
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t
	company-require-match nil))


(use-package counsel-projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-other-file-alist '(("c" "h")
				      ("h" "c" "cc" "cpp")
				      ("cc" "h")
				      ("cpp" "h")))
  (general-define-key :prefix my-leader
  		      "pp" 'counsel-projectile-switch-project
  		      "pf" 'counsel-projectile-find-file
  		      "pd" 'counsel-projectile-find-dir
  		      "pk" 'projectile-kill-buffers
  		      "pt" 'projectile-find-other-file)

  ;; Don't slow Emacs to a crawl when working with TRAMP.
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory)
      ad-do-it))
  (projectile-mode))


(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (general-define-key :prefix my-leader "e" 'flycheck-list-errors)
  (setq flycheck-c/c++-gcc-executable "gcc-5")
  (setq flycheck-gcc-language-standard "c++14"))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

(use-package multi-term
  :ensure t
  :bind (:map term-mode-map
	      ("C-c C-d" . term-send-eof)
	      :map term-raw-map
	      ("C-c C-d" . term-send-eof))
  :init
  (general-define-key :prefix my-leader
		      "x" 'multi-term)
  (general-define-key :keymaps 'term-raw-map
		      :states '(normal insert)
		      "C-n" 'multi-term-next
		      "C-p" 'multi-term-prev
		      "C-d" 'term-send-eof)
  :config
  (setq multi-term-program "/bin/zsh"))


(use-package iedit
  :ensure t
  :preface
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if (or arg
	    (buffer-narrowed-p))
	(iedit-mode)
      (save-excursion
	(save-restriction
	  (widen)
	  ;; this function determines the scope of `iedit-start'.
	  (if iedit-mode
	      (iedit-done)
	    ;; `current-word' can of course be replaced by other
	    ;; functions.
	    (narrow-to-defun)
	    (iedit-start (iedit-regexp-quote (current-word)) (point-min) (point-max)))))))
  :config
  (general-define-key :prefix my-leader "r" 'iedit-dwim))

(use-package wgrep
  :ensure t
  :defer t
  :init
  (general-define-key :keymaps 'grep-mode-map
		      :states 'normal
		      "W" 'wgrep-change-to-wgrep-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode)
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load 'abbrev (diminish 'abbrev-mode)))

(use-package dired
  :preface
  (defun my/dired-parent-dir ()
    (interactive)
    (find-alternate-file ".."))
  :defines (dired-omit-files dired-omit-files-p)
  :bind (:map dired-mode-map
	      ("RET" . dired-find-alternate-file)
	      ("<return>" . dired-find-alternate-file)
	      ("a" . dired-find-file)
	      ("q" . kill-this-buffer)
	      ("n" . evil-search-next)
	      ("N" . evil-search-previous)
	      ("W" . wdired-change-to-wdired-mode)
	      ("<backspace>" . my/dired-parent-dir)
	      ("?" . evil-search-backward))
  :config
  (require 'dired-x)
  (setq dired-omit-files
	"^\\..*\\|^\\.?#\\|^\\.$\\|^\\.\\.$")
  (setq dired-omit-files-p t)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-dwim-target t
  	dired-recursive-copies 'always
        dired-recursive-deletes 'always
  	delete-by-moving-to-trash t)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
	      ("j" . ibuffer-forward-line)
	      ("k" . ibuffer-backward-line)
	      ("q" . kill-this-buffer)))

(use-package proced
  :bind (:map proced-mode-map
	      ("j" . next-line)
	      ("k" . previous-line)))

(use-package magit
  :ensure t
  :bind (:map magit-status-mode-map
	      ("q" . kill-this-buffer)
	      ("j" . next-line)
	      ("k" . previous-line)
	      ("K" . magit-discard))
  :diminish auto-revert-mode
  :init
  (general-define-key :prefix my-leader
		      "g" 'magit-status))

(use-package ivy
  :ensure t
  :ensure smex
  :ensure counsel
  :diminish ivy-mode
  :bind (("M-x" . counsel-M-x)
	 :map ivy-mode-map
	 ("<escape>" . minibuffer-keyboard-quit))
  :config
  (setq projectile-completion-system 'ivy
	ivy-height 15
	ivy-count-format "(%d/%d) "
	ivy-display-style 'fancy)
  (general-define-key :prefix ","
   "f" 'counsel-find-file
   "F" 'counsel-recentf
   "b" 'ivy-switch-buffer
   "c" 'counsel-imenu
   "pg" 'counsel-git-grep)
  (ivy-mode 1))

(use-package nlinum-relative
  :ensure t
  :config
  (setq nlinum-relative-redisplay-delay 0.05
	nlinum-format " %d")
  (add-hook 'html-mode-hook #'nlinum-relative-mode)
  (add-hook 'prog-mode-hook #'nlinum-relative-mode))

(use-package hl-line
  :commands hl-line-mode
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'html-mode-hook #'hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))

(use-package buffer-move
  :ensure t
  :bind (:map evil-normal-state-map
	      ("C-S-h" . buf-move-left)
	      ("C-S-j" . buf-move-down)
	      ("C-S-k" . buf-move-up)
	      ("C-S-l" . buf-move-right)
	      ("C-M-S-h" . shrink-window-horizontally)
	      ("C-M-S-j" . shrink-window)
	      ("C-M-S-k" . enlarge-window)
	      ("C-M-S-l" . enlarge-window-horizontally)))

(use-package transpose-frame
  :ensure t
  :defer t
  :init
  (general-define-key :prefix my-leader "T" 'transpose-frame))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; C/C++ SETTINGS
(defvar current-brace-style 'own-line
  "Sets the brace style for yasnippet.")

(defun toggle-brace-style ()
  "Toggle the current C/C++ brace style."
  (interactive)
  (if (eq current-brace-style 'own-line)
      (setq current-brace-style 'same-line)
    (setq current-brace-style 'own-line)))

(defun insert-brace ()
  "Insert brace matching current brace style."
  (interactive)
  (if (eq current-brace-style 'own-line)
      (insert "\n{")
    (insert "{")))

(use-package c++-mode
  :mode (("\\.h\\'" . c++-mode))
  :init
  (setq c-basic-offset 4
	gdb-many-windows t
	c-default-style "bsd"))


;; RTAGS must be placed before irony for them to work together
(use-package rtags
  :ensure t
  :defer t
  :preface
  (defun rtags-add-project ()
    "Add project to RTags daemon."
    (interactive)
    (shell-command (concat "rc -J " (projectile-project-root))))
  :init
  (add-hook 'c-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook #'rtags-start-process-unless-running)
  (general-define-key :keymaps '(c-mode-map c++-mode-map)
		      :states 'normal
		      "M-." 'rtags-find-symbol-at-point
		      "M-," 'rtags-find-references-at-point
		      "R" 'rtags-rename-symbol)
  (general-define-key :keymaps '(c-mode-map c++-mode-map)
		      :states 'normal
		      :prefix my-leader
		      "R" 'rtags-rename-symbol)
  (general-define-key :keymaps 'rtags-mode-map
		      :states 'normal
		      "<return>" 'rtags-select-other-window
		      "q" 'kill-this-buffer))


(use-package irony
  :ensure t
  :defer t
  :diminish irony-mode
  :preface
  (defun my/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'my/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (setq irony-additional-clang-options '("-std=c++14")))

(use-package company-irony
  :ensure t
  :after irony
  :config (add-to-list 'company-backends '(company-irony)))

(use-package flycheck-irony
  :ensure t
  :after irony
  :config (flycheck-irony-setup))

(use-package company-irony-c-headers
  :ensure t
  :after irony
  :config (add-to-list 'company-backends '(company-irony-c-headers)))

(use-package irony-eldoc
  :ensure t
  :after irony
  :config (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package company-jedi
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
  :config
  (general-define-key :keymaps 'python-mode-map "M-." 'jedi:goto-definition))

;; RUST SETTINGS
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save))

;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   :preface
;;   (defun my/rust-mode-hook ()
;;     (setq-local company-backends '(company-capf))
;;     (global-lsp-mode 1))
;;   :init
;;   (add-hook 'rust-mode-hook #'my/rust-mode-hook)
;;   :config
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states '(normal insert)
;; 		      "C-." 'company-complete)
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states 'normal
;; 		      :prefix my-leader
;; 		      "R" 'lsp-rename))



(use-package racer
    :ensure t
    :diminish racer-mode
    :after rust-mode
    :init
    (general-define-key :keymaps 'rust-mode-map
			:states 'normal
			"M-." 'racer-find-definition)
    (add-hook 'rust-mode-hook #'racer-mode))

(use-package flycheck-rust
    :ensure t
    :after rust-mode
    :config
    (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;; Web development
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'html-mode-hook #'emmet-mode)
  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'js2-jsx-mode-hook #'emmet-mode))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
	 ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-strict-missing-semi-warning nil)
  (add-hook 'js2-jsx-mode-hook (lambda () (flycheck-mode -1))))

(use-package tern
  :ensure t
  :ensure company-tern
  :diminish tern-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook #'tern-mode)
  (add-hook 'js2-mode-hook (lambda ()
			     (interactive)
			     (add-to-list 'company-backends 'company-tern)))
  (general-define-key :keymaps '(js2-mode-map js2-jsx-mode-map)
		      :states 'normal
		      "M-." 'tern-find-definition
		      "M-," 'tern-pop-find-definition)
  (general-define-key :prefix my-leader
		      :keymaps '(js2-mode-map js2-jsx-mode-map)
		      :states 'normal
		      "R" 'tern-rename-variable)
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package org
  :defer t
  :preface
  (defun my/org-latex-export ()
    (interactive)
    (save-buffer)
    (org-latex-export-to-pdf)
    (let ((org-buffer (buffer-name))
	  (pdf-buffer (replace-regexp-in-string "\.org$" ".pdf" (buffer-name))))
      (if (get-buffer pdf-buffer)
	  (progn
	    (switch-to-buffer-other-window pdf-buffer)
	    (revert-buffer t t)
	    (switch-to-buffer-other-window org-buffer))
	(find-file-other-window (replace-regexp-in-string "\.org$" ".pdf" (buffer-file-name))))))
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (general-define-key :keymaps 'org-mode-map
		      "M-l" 'my/org-latex-export))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package ox-latex
  :defer t
  :config
  (add-to-list 'org-latex-classes
	       '("koma-article"
		 "\\documentclass{scrartcl}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-pdf-process
  	'("xelatex -interaction nonstopmode %f"
  	  "xelatex -interaction nonstopmode %f"))
  (setq org-latex-listings 'listings
	org-export-with-sub-superscripts nil
	org-export-with-smart-quotes t)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color")))

;; PDF-tools requires installation with (pdf-tools-install) first time it is used
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
	      ("C-h" . windmove-left)
	      ("C-j" . windmove-down)
	      ("C-k" . windmove-up)
	      ("C-l" . windmove-right)
	      ("j" . pdf-view-next-line-or-next-page)
	      ("k" . pdf-view-previous-line-or-previous-page)
	      ("M-j" . pdf-view-next-page)
	      ("M-k" . pdf-view-previous-page)))

(use-package tramp
  :defer t
  :config
  (setq tramp-verbose 2))


;; Escape quits everything
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; never shrink windows
(defvar allow-window-shrinking nil
  "If non-nil, effectively disable shrinking windows by making `shrink-window-if-larger-than-buffer' a no-op.")
(advice-add 'shrink-window-if-larger-than-buffer
            :before-while
            (lambda (&rest args)
              "Do nothing if `allow-window-shrinking' is nil."
              allow-window-shrinking))


(use-package workgroups2
  :ensure t
  :config
  (general-define-key :prefix ",w"
		      "c" 'wg-create-workgroup
		      "d" 'wg-delete-workgroup
		      "D" 'wg-delete-other-workgroups
		      "b" 'wg-switch-to-workgroup)
  (setq wg-session-load-on-start nil
	wg-emacs-exit-save-behavior nil
	wg-workgroups-mode-exit-save-behavior nil
	wg-session-file "~/.emacs.d/workgroups")

  (workgroups-mode 1))

(provide 'init)
;;; init.el ends here
