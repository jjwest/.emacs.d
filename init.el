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

;; General settings and better defaults
(setq initial-major-mode 'fundamental-mode
      auto-save-default nil
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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
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
(prefer-coding-system 'utf-8)
(setq-default cursor-in-non-selected-windows nil
	      fill-column 80)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-!") #'async-shell-command)
(global-set-key (kbd "M-%") #'shell-command)

;; Don't litter my init file
(setq custom-file "~/.emacs.d/local/custom-set.el")

;; Set font
(when (member "Source Code Pro" (font-family-list))
  (if (equal (display-pixel-height) 1440)
      (set-face-attribute 'default nil
			  :family "Source Code Pro"
			  :foundry 'ADBO
			  :slant 'normal
			  :weight 'normal
			  :height 113
			  :width 'normal)
    (set-face-attribute 'default nil
			:family "Source Code Pro"
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
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)


;; My own convenience functions
(defun my/split-line ()
  "Split line at point."
  (interactive)
  (newline-and-indent)
  (forward-line -1)
  (move-end-of-line 1))

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

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn)
  (custom-theme-set-faces
    'zenburn
    `(fringe ((t (:foreground  "#3F3F3F" :background "#3F3F3F"))))))

(use-package airline-themes
  :ensure powerline-evil
  :load-path "~/.emacs.d/themes"
  :diminish powerline-minor-modes
  :init
  (unless (file-exists-p "~/.emacs.d/themes/airline-themes.elc")
    (byte-recompile-directory "~/.emacs.d/themes" 0))
  :config
  (setq powerline-utf-8-separator-left        #xe0b0
	powerline-utf-8-separator-right       #xe0b2
	airline-utf-glyph-separator-left      #xe0b0
	airline-utf-glyph-separator-right     #xe0b2
	airline-utf-glyph-subseparator-left   #xe0b1
	airline-utf-glyph-subseparator-right  #xe0b3
	airline-utf-glyph-branch              #xe0a0
	airline-utf-glyph-readonly            #xe0a2
	airline-utf-glyph-linenumber          #xe0a1
	airline-helm-colors nil
	airline-display-directory nil
	airline-cursor-colors nil)
  (load-theme 'airline-base16-gui-dark))


;; Packages
(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "f" 'counsel-find-file
    "F" 'counsel-recentf
    "b" 'ivy-switch-buffer
    "B" 'ibuffer
    "k" 'kill-this-buffer
    "c" 'counsel-imenu
    "P" 'proced
    "pp" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "pd" 'counsel-projectile-find-dir
    "pk" 'projectile-kill-buffers
    "pt" 'projectile-find-other-file
    "pg" 'counsel-git-grep
    "ss" 'my/split-window-horizontal
    "vv" 'my/split-window-vertical
    "dw" 'delete-window
    "do" 'delete-other-windows
    "sf" 'save-buffer
    "sa" (lambda () (interactive) (save-some-buffers t))
    "g" 'magit-status
    "x" 'multi-term
    "W" 'winner-undo)
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("j" . evil-next-visual-line)
	      ("k" . evil-previous-visual-line)
	      ("TAB" . indent-for-tab-command)
	      ("C-h" . evil-window-left)
	      ("C-j" . evil-window-down)
	      ("C-k" . evil-window-up)
	      ("C-l" . evil-window-right)
	      ("M-k" . evil-scroll-up)
	      ("M-j" . evil-scroll-down)
	      ("C-a" . beginning-of-line)
	      ("C-q" . evil-scroll-line-up)
	      ("C-e" . evil-scroll-line-down)
	      ("S" . my/split-line)
	      ("U" . redo)
	      ("Q" . "@q")
	      ("Y" . "y$")
	      :map evil-visual-state-map
	      ("TAB" . indent-for-tab-command)
	      :map package-menu-mode-map
	      ("j" . evil-next-visual-line)
	      ("k" . evil-previous-visual-line))
  :config
  (setq evil-insert-state-cursor '(box))
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (evil-mode 1)

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

  (use-package evil-visualstar
    :ensure t
    :config (global-evil-visualstar-mode)))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'html-mode-hook #'yas-minor-mode)
  :config
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
  :bind (("C-." . company-manual-begin)
	 :map company-active-map
	 ("<C-return>" . my/company-abort-and-newline)
	 ("<tab>" . nil))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t
	company-require-match nil))


(use-package counsel-projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (setq projectile-other-file-alist '(("c" "h")
				      ("h" "c" "cc" "cpp")
				      ("cc" "h")
				      ("cpp" "h")))
  (projectile-mode))


(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
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
  (evil-define-key 'normal term-raw-map (kbd "C-n") #'multi-term-next)
  (evil-define-key 'normal term-raw-map (kbd "C-p") #'multi-term-prev)
  (evil-define-key 'normal term-raw-map (kbd "C-d") #'term-send-eof)
  (evil-define-key 'insert term-raw-map (kbd "C-d") #'term-send-eof)
  :config
  (setq multi-term-program "/bin/zsh"))


(use-package iedit
  :ensure t
  :preface
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
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
  (evil-leader/set-key "r" 'iedit-dwim))

(use-package wgrep
  :ensure t
  :defer t
  :init
  (evil-leader/set-key-for-mode 'grep-mode "W" 'wgrep-change-to-wgrep-mode))

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
  :bind (:map dired-mode-map
	      ("RET" . dired-find-alternate-file)
	      ("<return>" . dired-find-alternate-file)
	      ("a" . dired-find-file)
	      ("q" . kill-this-buffer)
	      ("W" . wdired-change-to-wdired-mode)
	      ("<backspace>" . my/dired-parent-dir)
	      ("?" . evil-search-backward))
  :config
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
  :diminish auto-revert-mode)

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
  (evil-leader/set-key "T" 'transpose-frame))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; C++ SETTINGS
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
  (defun my/rtags-add-project ()
    "Add project to RTags daemon."
    (interactive)
    (shell-command (concat "rc -J " (projectile-project-root))))
  :init
  (add-hook 'c-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook #'rtags-start-process-unless-running)
  :config
  (evil-leader/set-key-for-mode 'c-mode "R" 'rtags-rename-symbol)
  (evil-leader/set-key-for-mode 'c++-mode "R" 'rtags-rename-symbol)
  (evil-define-key 'normal rtags-mode-map (kbd "<return>") #'rtags-select-other-window)
  (evil-define-key 'normal rtags-mode-map (kbd "q") #'kill-this-buffer)
  (evil-define-key 'normal c-mode-map (kbd "M-.") #'rtags-find-symbol-at-point)
  (evil-define-key 'normal c-mode-map (kbd "M-,") #'rtags-find-references-at-point)
  (evil-define-key 'normal c++-mode-map (kbd "M-.") #'rtags-find-symbol-at-point)
  (evil-define-key 'normal c++-mode-map (kbd "M-,") #'rtags-find-references-at-point))

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
  (add-hook 'python-mode-hook '(lambda () (add-to-list 'company-backends 'company-jedi)))
  :config
  (evil-define-key 'normal python-mode-map (kbd "M-.") #'jedi:goto-definition))

;; RUST SETTINGS
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package racer
    :ensure t
    :diminish racer-mode
    :after rust-mode
    :config
    (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
    (setq racer-rust-src-path (expand-file-name "~/.rust/src"))
    (setq racer-cargo-home (expand-file-name "~/.cargo"))
    (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition)
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
  :mode (("\\.js\\'" . js2-jsx-mode)
	 ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (add-hook 'js2-jsx-mode-hook (lambda () (flycheck-mode -1))))

(use-package company-tern
  :ensure t
  :diminish tern-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook #'tern-mode)
  (add-hook 'js2-jsx-mode-hook #'tern-mode)
  (evil-define-key 'normal js2-mode-map (kbd "M-.") 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map (kbd "M-,") 'tern-pop-find-definition)
  (evil-define-key 'normal js2-jsx-mode-map (kbd "M-.") 'tern-find-definition)
  (evil-define-key 'normal js2-jsx-mode-map (kbd "M-,") 'tern-pop-find-definition)
  (evil-leader/set-key-for-mode 'js2-jsx-mode
    "R" 'tern-rename-variable)
  (evil-leader/set-key-for-mode 'js2-mode
    "R" 'tern-rename-variable)
  :config
  (add-to-list 'company-backends 'company-tern))


(use-package omnisharp
  :ensure t
  :defer t
  :init
  (add-hook 'csharp-mode-hook #'omnisharp-mode)
  (evil-define-key 'normal csharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
  (evil-define-key 'normal csharp-mode-map (kbd "M-,") 'omnisharp-find-usages)
  (evil-leader/set-key-for-mode 'csharp-mode
    "R" 'omnisharp-rename)
  :config
  (setq omnisharp-server-executable-path "/opt/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
	omnisharp-curl-executable-path "/usr/bin/curl")
  (add-to-list 'company-backends 'company-omnisharp))

(use-package org
  :ensure t
  :defer t
  :preface
  (defun my/org-latex-export ()
    (interactive)
    (save-buffer)
    (org-latex-export-to-pdf)
    (let ((org-buffer (buffer-name))
	  (pdf-buffer (replace-regexp-in-string "\.org" ".pdf" (buffer-name))))
      (if (get-buffer pdf-buffer)
	  (progn
	    (switch-to-buffer pdf-buffer)
	    (revert-buffer t t)
	    (switch-to-buffer org-buffer))
	(find-file-other-window (replace-regexp-in-string "\.org" ".pdf" (buffer-file-name))))))
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'my/org-latex-export))

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
	 ("j" . pdf-view-next-line-or-next-page)
	 ("k" . pdf-view-previous-line-or-previous-page)
	 ("M-j" . pdf-view-next-page)
	 ("M-k" . pdf-view-previous-page)))

(use-package tramp
  :defer t
  :config
  (setq tramp-verbose 2))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

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
