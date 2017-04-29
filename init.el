; init.el --- My Emacs config
;;; Commentary:
;;; Nothing here
;;; Code:

;; Better garbage collection settings
(setq gc-cons-threshold (* 20 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Benchmark startup time
(use-package benchmark-init
  :ensure t
  :config (benchmark-init/activate))

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
      tab-width 4
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

(defun my/save-all-buffers ()
  "Save all buffers without prompt."
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

(defvar current-brace-style 'own-line
  "Sets the brace style for yasnippet.")

(defun toggle-brace-style ()
  "Toggle the current brace style."
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

(defun change-theme (theme)
  (interactive
   (list
    (intern (completing-read "Change to theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme))

;; Packages
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

(use-package doom-themes
  :ensure t
  :preface
  (defun tweak-doom-theme (&rest args)
    (when (member 'doom-one custom-enabled-themes)
      (custom-theme-set-faces
       'doom-one
       `(doom-linum
	 ((((type graphic)) :inherit linum :foreground "#5B6268" :background "#282c34")
	  (t                :inherit linum)))
       `(nlinum-relative-current-face ((t (:foreground "#46D9FF" :bold t)))))))
  :init
  (advice-add #'change-theme :after #'tweak-doom-theme)
  (advice-add #'load-theme :after #'tweak-doom-theme)
  :config
  (load-theme 'doom-one)
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer))

(use-package solarized-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t
  :preface
  (defun tweak-spacemacs-theme (&rest args)
    (when (member 'spacemacs-light custom-enabled-themes)
      (custom-theme-set-faces
       'spacemacs-light
       `(nlinum-relative-current-face ((t (:foreground "#efeae9" :background "#a8a8bf" :bold t)))))
      (with-eval-after-load 'nlinum-relative
	(setq nlinum-format " %d "))))
  :init
  (advice-add #'change-theme :after #'tweak-spacemacs-theme)
  (advice-add #'load-theme :after #'tweak-spacemacs-theme))

(use-package doom-modeline
  :ensure powerline
  :ensure s
  :ensure f
  :ensure evil
  :ensure projectile
  :load-path "~/.emacs.d/lisp"
  :init
  (unless (file-exists-p "~/.emacs.d/lisp/doom-modeline.elc")
    (byte-compile-file "~/.emacs.d/lisp/doom-modeline.el")))

(use-package doom-vcs
  :load-path "~/.emacs.d/lisp"
  :init
  (unless (file-exists-p "~/.emacs.d/lisp/doom-vcs.elc")
    (byte-compile-file "~/.emacs.d/lisp/doom-vcs.el"))
  (setq-default fringes-outside-margins t))

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
  (general-define-key :keymaps 'global
		      "C-h" #'windmove-left
		      "C-j" #'windmove-down
		      "C-k" #'windmove-up
		      "C-l" #'windmove-right)
  (general-define-key :keymaps 'evil-visual-state-map
  		      "TAB" 'indent-for-tab-command)
  (general-define-key :keymaps 'package-menu-mode-map
		      "j" 'evil-next-visual-line
		      "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

(use-package evil-exchange
  :ensure t
  :config (evil-exchange-install))

(use-package evil-args
  :ensure t
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg))

(use-package evil-cleverparens
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  :config
  (general-define-key :keymaps 'evil-cleverparens-mode-map
		      :states 'normal
		      "S" nil
		      "M-j" nil
		      "M-k" nil
		      "d" nil))

(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode))

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
  :defer t
  :init
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
  :config
  (projectile-mode))


(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (general-define-key :prefix my-leader "e" 'flycheck-list-errors)
  (setq flycheck-c/c++-gcc-executable "gcc-5")
  (setq-default flycheck-gcc-language-standard "c++14")
  (setq-default flycheck-clang-language-standard "c++14"))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

(use-package multi-term
  :ensure t
  :preface
  (defun buffer-is-term-p (buf)
    (s-matches-p (rx "*terminal<" digit ">*")
		 buf))

  (defun toggle-terminal ()
    (interactive)
    (if (buffer-is-term-p (buffer-name))
	(progn
	  (while (buffer-is-term-p (buffer-name))
	    (switch-to-prev-buffer))
	  (other-window 1))
      (let* ((open-buffers (mapcar 'buffer-name (buffer-list)))
	     (term-buffers (cl-remove-if-not 'buffer-is-term-p open-buffers)))
	(if term-buffers
	    (switch-to-buffer-other-window (car term-buffers))
	  (when (= (count-windows) 1)
	    (split-window-sensibly))
	  (other-window 1)
	  (multi-term)))))
  :bind (:map term-mode-map
	      ("C-c C-d" . term-send-eof)
	      :map term-raw-map
	      ("C-c C-d" . term-send-eof))
  :init
  (general-define-key :prefix my-leader
		      "x" 'toggle-terminal
		      "X" 'multi-term)
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
  :init
  (setq projectile-completion-system 'ivy
	ivy-height 15
	ivy-count-format "(%d/%d) "
	ivy-display-style 'fancy)
  (general-define-key :prefix my-leader
   "f" 'counsel-find-file
   "F" 'counsel-recentf
   "b" 'ivy-switch-buffer
   "c" 'counsel-imenu
   "pg" 'counsel-git-grep)
  (ivy-mode 1))

(use-package nlinum-relative
  :ensure t
  :init
  (setq nlinum-relative-redisplay-delay 0.05
	nlinum-format "%d ")
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
		      "M-," 'rtags-location-stack-back
		      "M--" 'rtags-find-all-references-at-point
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
      'irony-completion-at-point-async)
    (irony-cdb-autosetup-compile-options))

  (defun my/irony-cleanup ()
    (when (get-process "Irony")
      (let ((c-cpp-buffers (--filter
			    (with-current-buffer it
			      (or (eq major-mode 'c-mode)
				  (eq major-mode 'c++-mode)))
			    (buffer-list))))
	(unless c-cpp-buffers
	  (irony-server-kill)))))
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'my/irony-mode-hook)
  :config
  (setq irony-additional-clang-options '("-std=c++14"))
  (add-hook 'kill-buffer-hook #'my/irony-cleanup))


(use-package company-irony
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-to-list 'company-backends 'company-irony)))

(use-package flycheck-irony
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (flycheck-irony-setup)))

(use-package company-irony-c-headers
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-to-list 'company-backends 'company-irony-c-headers)))

(use-package irony-eldoc
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package company-jedi
  :ensure t
  :defer t
  :preface
  (defun my/init-python-hook ()
    (add-to-list 'company-backends 'company-jedi)
    (jedi:setup))
  :init
  (add-hook 'python-mode-hook #'my/init-python-hook)
  (general-define-key :keymaps 'python-mode-map
		      :states 'normal
		      "M-." 'jedi:goto-definition
		      "M-," 'jedi:goto-definition-pop-marker))

;; RUST SETTINGS
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save))


;; (use-package lsp-mode
;;   :ensure t
;;   :after rust-mode
;;   :config
;;   (let* ((sysroot (s-trim-right
;; 		   (shell-command-to-string "rustc --print sysroot")))
;; 	 (lib (f-join sysroot
;; 		      "lib")))
;;     (setenv "LD_LIBRARY_PATH" lib))
;;   (global-lsp-mode 1)
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states '(normal insert)
;; 		      "C-." 'company-complete)
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states 'normal
;; 		      :prefix my-leader
;; 		      "R" 'lsp-rename))


;; (use-package lsp-rust
;;   :ensure t)

(use-package racer
    :ensure t
    :diminish racer-mode
    :defer t
    :init
    (general-define-key :keymaps 'rust-mode-map
			:states 'normal
			"M-," #'pop-tag-mark
			"M-." #'racer-find-definition)
    (add-hook 'rust-mode-hook #'racer-mode))

(use-package cider
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  :config
  (general-define-key :keymaps 'clojure-mode-map
		      :states 'normal
		      "C-r" #'cider-run
		      "C-f" #'cider-eval-buffer))

(use-package flycheck-rust
    :ensure t
    :defer t
    :init
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
  :mode ("\\.org\\'" . org-mode)
  :defines org-export-async-init-file
  :preface
  (defun my/org-latex-export ()
    (interactive)
    (save-buffer)
    (org-latex-export-to-pdf t))
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (setq org-export-async-init-file (f-join user-emacs-directory
  					   "lisp"
  					   "org-export.el"))
  (setq org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (general-define-key :keymaps 'org-mode-map
		      "M-l" 'my/org-latex-export))

(use-package org-ref
  :ensure t
  :after org
  :preface
  (defun my/insert-space-before-cmd (&rest args)
    (goto-char (+ (point) 1))
    (insert " "))
  :config
  (require 'doi-utils)
  (advice-add #'org-ref-helm-insert-cite-link :before #'my/insert-space-before-cmd)
  (advice-add #'org-ref-helm-insert-ref-link :before #'my/insert-space-before-cmd)
  (advice-add #'org-ref-helm-insert-label-link :before #'my/insert-space-before-cmd)
  (general-define-key :prefix my-leader
		      :keymaps 'org-mode-map
		      :states 'normal
		      "R" #'org-ref-helm-insert-ref-link
		      "C" #'org-ref-helm-insert-cite-link
		      "L" #'org-ref-helm-insert-label-link))

(use-package darkroom
  :ensure t
  :after org
  :config
  (setq darkroom-margins 0.25)
  (add-hook 'org-mode-hook #'darkroom-mode))

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

(provide 'init)
;;; init.el ends here
