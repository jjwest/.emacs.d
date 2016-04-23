(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; General stuff
(setq initial-major-mode 'text-mode
      auto-save-default nil
      make-backup-files nil
      custom-safe-themes t
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      inhibit-startup-screen t
      initial-scratch-message ""
      tramp-default-method "ssh"
      indent-tabs-mode nil
      gdb-many-windows t
      c-basic-offset 4
      c-default-style "bsd")
(put 'dired-find-alternate-file 'disabled nil)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(electric-pair-mode 1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)


(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
  "f" 'find-file
  "b" 'switch-to-buffer
  "B" 'buffer-menu
  "k" 'kill-buffer
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pk" 'projectile-kill-buffers
  "pt" 'projectile-find-other-file
  "ss" 'split-window-horizontally
  "vv" 'split-window-vertically
  "dw"	'delete-window
  "do" 'delete-other-windows
  "sf" 'save-buffer
  "sa" 'save-some-buffers
  "g" 'magit-status
  "r" 'replace-string
  "R" 'ggtags-query-replace
  "x" 'ansi-term
  "jd" 'ggtags-find-definition
  "D" 'dired
  "c" 'idomenu)
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
	      ("M-j" . evil-scroll-down))
  :config
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'magit-mode 'emacs)
  (setq evil-move-cursor-back nil)
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package gruvbox-theme :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode)
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load	 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'abbrev (diminish 'abbrev-mode)))

(defun my-dired-parent-dir ()
  (interactive)
  (find-alternate-file ".."))
(use-package dired
  :bind (:map dired-mode-map
	      ("RET" . dired-find-alternate-file)
	      ("<return>" . dired-find-alternate-file)
	      ("a" . dired-find-file)
	      ("q" . kill-this-buffer)
	      ("p" . my-dired-parent-dir)
	      ("f" . isearch-forward)
	      ("F" . isearch-backward)))

(use-package term
  :bind ("C-x C-d" . term-send-eof))

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init (add-hook 'prog-mode-hook (lambda () (yas-minor-mode)))
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))


(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :bind (("C-RET" . company-manual-begin)
	 ("<C-return>" . company-manual-begin)
	 :map company-active-map
	 ("TAB" . nil)
	 ("<tab>" . nil))
  :init (add-hook 'prog-mode-hook (lambda () (company-mode)))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-clang-executable "/usr/bin/clang-3.7"))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-other-file-alist '(("c" "h")
				      ("h" "c" "cc" "cpp")
				      ("cc" "h")
				      ("cpp" "h")))
  (setq projectile-globally-ignored-file-suffixes '(".fls"
                                                    ".log"
                                                    ".fdb_latexmk"
                                                    ".aux"
                                                    ".dvi"))
  (projectile-global-mode))


(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-c/c++-gcc-executable "gcc-5")
  (setq flycheck-gcc-language-standard "c++14")
  (use-package flycheck-pos-tip
    :ensure t
    :config (flycheck-pos-tip-mode)))

;; PYTHON SETTINGS
(use-package company-jedi
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi))))

;; RUST SETTINGS
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'flycheck-rust-setup)))

(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode
  :init (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package magit
  :ensure t
  :defer t
  :bind (:map magit-status-mode-map
	      ("q" . kill-this-buffer))
  :diminish auto-revert-mode)

(use-package powerline-evil
  :ensure t
  :diminish powerline-minor-modes
  :config (powerline-evil-vim-color-theme))

(use-package ido
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (ido-mode 1)
  (use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode 1))
  (use-package ido-ubiquitous
    :ensure t
    :config (ido-ubiquitous-mode 1))
  (use-package idomenu :ensure t))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package irony
  :ensure t
  :defer t
  :diminish irony-mode
  :init
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'eldoc-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
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
    :config (add-hook 'irony-mode-hook 'irony-eldoc)))

(use-package linum-relative
  :ensure t
  :diminish
  linum-mode
  linum-relative
  linum-relative-mode
  :config
  (add-hook 'prog-mode-hook 'linum-relative-mode)
  (add-hook 'conf-mode-hook 'linum-relative-mode))

;; esc quits
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight semi-bold :height 98 :width normal))))
 '(linum ((t (:foreground "#666462"))))
 '(linum-relative-current-face ((t (:foreground "#a89984"))))
 '(term-color-blue ((t (:background "deep sky bluei" :foreground "cornflower blue"))))
 '(term-color-green ((t (:background "#aeee00" :foreground "#aeee00")))))

(provide 'init)
