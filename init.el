; init.el --- My Emacs config
;;; Commentary:
;;; Nothing here
;;; Code:

;; Better garbage collection settings
(setq gc-cons-threshold (* 20 1024 1024))
(add-hook 'focus-out-hook #'garbage-collect)

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
      make-backup-files nil
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
      require-final-newline t
      auto-revert-check-vc-info t
      show-paren-delay 0
      select-enable-clipboard t
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil
      locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default cursor-in-non-selected-windows nil
	      fill-column 80)
(set-frame-parameter nil 'fullscreen 'fullboth)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

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
(save-place-mode)
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

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn)
  (custom-theme-set-faces
    'zenburn
    `(fringe ((t (:foreground  "#3F3F3F" :background "#3F3F3F"))))))

(use-package powerline-evil
  :ensure t
  :diminish powerline-minor-modes
  :config
  (use-package airline-themes
    :load-path "~/.emacs.d/themes"
    :preface
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
    (load-theme 'airline-base16-gui-dark)))

;; Packages
(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "f" 'counsel-find-file
    "b" 'ivy-switch-buffer
    "B" 'ibuffer
    "k" 'kill-this-buffer
    "c" 'counsel-imenu
    "P" 'proced
    "pp" 'projectile-switch-project
    "pf" 'projectile-find-file
    "pk" 'projectile-kill-buffers
    "pt" 'projectile-find-other-file
    "pg" 'counsel-git-grep
    "ss" 'my/split-window-horizontal
    "vv" 'my/split-window-vertical
    "dw" 'delete-window
    "do" 'delete-other-windows
    "sf" 'save-buffer
    "sa" '(lambda () (interactive) (save-some-buffers t))
    "g" 'magit-status
    "x" '(lambda () (interactive) (ansi-term "/bin/zsh"))
    "W" 'winner-undo)
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("j" . evil-next-visual-line)
	      ("k" . evil-previous-visual-line)
	      ("C-h" . evil-window-left)
	      ("C-j" . evil-window-down)
	      ("C-k" . evil-window-up)
	      ("C-l" . evil-window-right)
	      ("M-k" . evil-scroll-up)
	      ("M-j" . evil-scroll-down)
	      ("C-a" . beginning-of-line)
	      ("C-e" . end-of-line)
	      ("S" . my/split-line)
	      ("U" . redo)
	      ("Q" . "@q")
	      ("Y" . "y$")
	      :map package-menu-mode-map
	      ("j" . evil-next-visual-line)
	      ("k" . evil-previous-visual-line))
  :config
  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1))

  (use-package evil-visualstar
    :ensure t
    :config (global-evil-visualstar-mode))

  (setq evil-insert-state-cursor '(box))
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (evil-mode 1))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))


(use-package company
  :ensure t
  :diminish company-mode
  :commands (company-mode)
  :bind (("C-RET" . company-manual-begin)
	 ("<C-return>" . company-manual-begin)
	 :map company-active-map
	 ("TAB" . nil)
	 ("<tab>" . nil))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t))


(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (setq projectile-other-file-alist '(("c" "h")
				      ("h" "c" "cc" "cpp")
				      ("cc" "h")
				      ("cpp" "h")))
  (projectile-global-mode))


(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-c/c++-gcc-executable "gcc-5")
  (setq flycheck-gcc-language-standard "c++14")
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (setq flycheck-pos-tip-timeout 30)
    (flycheck-pos-tip-mode)))

(use-package evil-anzu
  :ensure t
  :diminish anzu-mode
  :config
  (evil-leader/set-key
    "r" 'anzu-query-replace-at-cursor
    "R" 'anzu-query-replace)
  (global-anzu-mode))

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
  :bind (:map dired-mode-map
	      ("RET" . dired-find-alternate-file)
	      ("<return>" . dired-find-alternate-file)
	      ("a" . dired-find-file)
	      ("j" . dired-next-line)
	      ("k" . dired-previous-line)
	      ("q" . kill-this-buffer)
	      ("<backspace>" . my/dired-parent-dir)
	      ("/" . evil-search-forward)
	      ("?" . evil-search-backward)
	      ("n" . evil-search-next)
	      ("N" . evil-search-previous))
  :init
  (defun my/dired-parent-dir ()
    (interactive)
    (find-alternate-file ".."))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always
	dired-recursive-copies 'always
	delete-by-moving-to-trash t))

(use-package term
  :bind ("C-x C-d" . term-send-eof)
  :config (setq term-buffer-maximum-size 0))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
	      ("j" . ibuffer-forward-line)
	      ("k" . ibuffer-backward-line)))

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
  :functions minibuffer-keyboard-quit
  :diminish ivy-mode
  :bind (("M-x" . counsel-M-x)
	 :map ivy-mode-map
	 ("<escape>" . minibuffer-keyboard-quit))
  :demand
  :config
  (setq projectile-completion-system 'ivy
	ivy-use-virtual-buffers t
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
  :ensure t
  :commands hl-line-mode
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'html-mode-hook #'hl-line-mode))

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
  :init
  (defun my/rtags-add-project ()
    "Add project to the RTags daemon."
    (interactive)
    (shell-command (concat "rc -J " (projectile-project-root))))
  (add-hook 'c-mode-hook #'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook #'rtags-start-process-unless-running)
  :config
  (evil-leader/set-key-for-mode 'c-mode
    "R" 'rtags-rename-symbol)
  (evil-leader/set-key-for-mode 'c++-mode
    "R" 'rtags-rename-symbol)
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
  :init
  (defun my/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'my/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
  (setq irony-additional-clang-options '("-std=c++14"))
  (use-package company-irony
    :ensure t
    :config (add-to-list 'company-backends '(company-irony)))
  (use-package flycheck-irony
    :ensure t
    :config (flycheck-irony-setup))
  (use-package company-irony-c-headers
    :ensure t
    :config (add-to-list 'company-backends '(company-irony-c-headers)))
  (use-package irony-eldoc
    :ensure t
    :config (add-hook 'irony-mode-hook #'irony-eldoc)))

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
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (use-package racer
    :ensure t
    :diminish racer-mode
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    :config
    (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
    (setq racer-rust-src-path (expand-file-name "~/.rust/src"))
    (setq racer-cargo-home (expand-file-name "~/.cargo"))
    (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition))

  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'flycheck-rust-setup)))


;; Web development
(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'html-mode-hook #'emmet-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode))

(use-package company-tern
  :ensure t
  :diminish tern-mode
  :defer t
  :init (add-hook 'js2-mode-hook #'tern-mode)
  :config
  (add-to-list 'company-backends 'company-tern)
  (evil-define-key 'normal js2-mode-map (kbd "M-.") 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map (kbd "M-,") 'tern-pop-find-definition)
  (evil-leader/set-key-for-mode 'js2-mode
    "R" 'tern-rename-variable))

(use-package ibuffer
  :ensure t
  :config
  (add-to-list 'ibuffer-fontification-alist '(5 buffer-file-name 'font-lock-keyword-face)))


(use-package org
  :ensure t
  :defer t
  :init
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
  (add-hook 'org-mode-hook #'org-indent-mode)
  :config
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook #'org-bullets-mode))
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'my/org-latex-export))

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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight regular :height 98 :width normal)))))

(provide 'init)
