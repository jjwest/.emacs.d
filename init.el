; init.el --- My Emacs config
;;; Commentary:
;;; Nothing here
;;; Code:

;; Better garbage collection settings
(setq gc-cons-threshold (* 20 1024 1024))

(defun inhibit-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun resume-gc ()
  (setq gc-cons-threshold (* 20 1024 1024)))

(add-hook 'minibuffer-setup-hook #'inhibit-gc)
(add-hook 'minibuffer-exit-hook #'resume-gc)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-demand t)

;; Benchmark startup time
(use-package benchmark-init
  :ensure t
  :config (benchmark-init/activate))

;; General settings and better defaults
(setq custom-safe-themes t
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1
      inhibit-startup-screen t
      initial-scratch-message ""
      load-prefer-newer t
      ad-redefinition-action 'accept
      uniquify-buffer-name-style 'forward
      message-log-max 200
      bidi-paragraph-direction 'left-to-right
      auto-revert-check-vc-info t
      require-final-newline nil
      show-paren-delay 0
      save-interprogram-paste-before-kill t
      select-enable-clipboard t
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil
      locale-coding-system 'utf-8)

(setq-default tab-width 4
			  indent-tabs-mode nil)

;; I like my backups hidden and in abundance
(unless (file-exists-p "~/.emacs.d/backups")
  (mkdir "~/.emacs.d/backups/per-save" t)
  (mkdir "~/.emacs.d/backups/per-session" t))

 (setq backup-directory-alist '(("" . "~/.emacs.d/backups/per-save"))
       backup-by-copying t
       delete-old-versions t
       kept-new-versions 20
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
(setq frame-title-format "Emacs: %b")
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq-default cursor-in-non-selected-windows nil
	      fill-column 80)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default isearch-allow-scroll t
	      lazy-highlight-cleanup nil
	      lazy-highlight-initial-delay 0)


;; Don't litter my init file
(unless (file-exists-p "~/.emacs.d/local")
  (mkdir "~/.emacs.d/local"))
(setq custom-file "~/.emacs.d/local/custom-set.el")

(defun set-font-on-start (frame)
  (select-frame frame)
  (when (member "Office Code Pro" (font-family-list))
    (set-frame-font "Office Code Pro-11" t t))
  (remove-hook 'after-make-frame-functions 'set-font-on-start))

;; Set font
(if (daemonp)
    (add-hook 'after-make-frame-functions #'set-font-on-start)
  (when (member "Office Code Pro" (font-family-list))
    (set-frame-font "Office Code Pro-11" t t)))


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
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(display-time)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (>= emacs-major-version 26)
  (add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative))))

(defun maybe-kill-buffers (frame)
  "Kill all live buffers when the last frame is closed."
  (when (<= (length (frame-list)) 2)
    (mapc #'kill-buffer (buffer-list))))

(add-hook 'delete-frame-functions #'maybe-kill-buffers)

;; Convenience functions
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

;; ;; Ugly hack to ensure the modeline loads properly
;; ;; for Message and scratch buffer
(when (daemonp)
  (with-eval-after-load 'doom-modeline
    (kill-buffer "*scratch*")
    (kill-buffer "*Messages*")
    (add-hook 'kill-buffer-query-functions #'my/dont-kill-scratch)
    (switch-to-buffer "*Messages*")
    (switch-to-buffer "*scratch*")))

;; Always give new frames focus
(when (daemonp)
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (select-frame-set-input-focus frame))))

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
    (setq current-brace-style 'own-line))
  (message "Brace style set to %s" current-brace-style))

(defun insert-brace ()
  "Insert brace matching current brace style."
  (interactive)
  (if (eq current-brace-style 'own-line)
      (insert "\n{")
    (insert "{")))

(defun change-theme (theme)
  "Change theme to THEME, disabling all current themes first."
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
		      "k" 'kill-current-buffer
		      "B" 'ibuffer
		      "P" 'proced
		      "W" 'winner-undo
		      "ss" 'my/split-window-horizontal
		      "vv" 'my/split-window-vertical))

(use-package doom-common
  :ensure s
  :ensure f
  :ensure all-the-icons
  :ensure evil
  :ensure projectile
  :load-path "~/.emacs.d/lisp")

(use-package doom-themes
  :ensure t
  :preface
  (defun tweak-doom-theme (&rest args)
    (when (member 'doom-one custom-enabled-themes)
      (solaire-mode-swap-bg)
      (custom-theme-set-faces
       'doom-one
       `(git-gutter:modified ((t (:foreground "#ECBE7B"))))
       `(git-gutter-fr:modified ((t (:foreground "#ECBE7B"))))
       `(font-lock-preprocessor-face ((t (:foreground "#DA8548" :bold t))))
       `(line-number ((t (:foreground "#5B6268"))))
       `(fringe ((t (:inherit solaire-default-face))))
       `(font-lock-variable-name-face ((t (:foreground "#DFDFDF")))))))

  (defun load-doom-theme (frame)
    (select-frame frame)
    (load-theme 'doom-one)
    (remove-hook 'after-make-frame-functions 'load-doom-theme))
  :init
  (advice-add #'load-theme :after #'tweak-doom-theme)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-doom-theme)
    (load-theme 'doom-one)))



(use-package doom-modeline
  :ensure powerline
  :defer t
  :load-path "~/.emacs.d/lisp"
  :preface
  (defun load-doom-modeline (frame)
    (select-frame frame)
    (require 'doom-modeline)
    (remove-hook 'after-make-frame-functions 'load-doom-modeline))
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-doom-modeline)
    (require 'doom-modeline))

  (with-eval-after-load 'doom-modeline
    (defadvice doom-buffer-path (around ignore-remote first activate)
      (if (file-remote-p default-directory)
	      (if buffer-file-name
	          (setq ad-return-value (file-name-nondirectory (buffer-file-name)))
	        (setq ad-return-value "%b"))
	    ad-do-it)))

  (unless (file-exists-p "~/.emacs.d/lisp/doom-modeline.elc")
    (byte-compile-file "~/.emacs.d/lisp/doom-modeline.el")))

(use-package doom-vcs
  :load-path "~/.emacs.d/lisp"
  :init
  (unless (file-exists-p "~/.emacs.d/lisp/doom-vcs.elc")
    (byte-compile-file "~/.emacs.d/lisp/doom-vcs.el"))
  (setq-default fringes-outside-margins t))

(use-package solaire-mode
  :ensure t
  :config
  (setq solaire-mode-remap-line-numbers t)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (advice-add #'solaire-mode :after
	      (lambda (&rest args)
		(custom-theme-set-faces
		 'doom-one
		 `(fringe ((t (:inherit solaire-default-face))))))))

(use-package evil
  :ensure t
  :config
  (general-define-key
   "j" #'evil-next-visual-line
   "k" #'evil-previous-visual-line
   "TAB" #'indent-for-tab-command
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "C-l" #'evil-window-right
   "M-k" #'evil-scroll-up
   "M-j" #'evil-scroll-down
   "C-a" #'beginning-of-line
   "C-q" #'evil-scroll-line-up
   "C-e" #'evil-scroll-line-down
   "S" #'my/split-line
   "U" #'redo
   "M-." #'xref-find-definitions
   "M-," #'xref-pop-marker-stack
   "M--" #'xref-find-references
   "Q" "@q"
   "Y" "y$")
  (general-define-key :keymaps 'global
		      "C-h" #'windmove-left
		      "C-j" #'windmove-down
		      "C-k" #'windmove-up
		      "C-l" #'windmove-right)
  (general-define-key :keymaps 'evil-visual-state-map
		      "j" #'evil-next-visual-line
		      "k" #'evil-previous-visual-line
  		      "TAB" #'indent-for-tab-command)
  (general-define-key :keymaps 'package-menu-mode-map
		      "j" #'evil-next-visual-line
		      "k" #'evil-previous-visual-line)
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

(use-package evil-iedit-state
  :ensure t)

(use-package evil-cleverparens
  :ensure t
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
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil
	company-require-match nil))


(use-package counsel-projectile
  :ensure t
  :diminish projectile-mode
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
  		      "pt" 'projectile-find-other-file
		      "pT" 'projectile-find-other-file-other-window)

  ;; Don't slow Emacs to a crawl when working with TRAMP.
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory)
      ad-do-it))
  :config
  (projectile-mode)
  (counsel-projectile-on))


(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (general-define-key "M-n" #'flycheck-next-error
		      "M-p" #'flycheck-previous-error)
  (setq-default flycheck-gcc-language-standard "c++14")
  (setq-default flycheck-clang-language-standard "c++14"))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-pos-tip-timeout 30)
  (flycheck-pos-tip-mode))

(use-package terminal-here
  :ensure t
  :config (general-define-key :prefix my-leader "x" #'terminal-here))

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

(use-package rg
  :ensure t)

(use-package wgrep
  :ensure t
  :init
  (general-define-key :prefix my-leader
		      :keymaps 'grep-mode-map
		      :states 'normal
		      "w" 'wgrep-change-to-wgrep-mode))

(use-package imenu
  :config
  (general-define-key :prefix my-leader "c" #'imenu))

(use-package eldoc
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
	      ("q" . kill-current-buffer)
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
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-details+
  :ensure t
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
	      ("j" . ibuffer-forward-line)
	      ("k" . ibuffer-backward-line)
	      ("q" . kill-current-buffer)))

(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-vc-set-filter-groups-by-vc-root)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(use-package proced
  :bind (:map proced-mode-map
	      ("j" . next-line)
	      ("k" . previous-line)))

(use-package magit
  :ensure t
  :bind (:map magit-status-mode-map
	      ("q" . kill-current-buffer)
	      ("j" . next-line)
	      ("k" . previous-line)
	      ("K" . magit-discard))
  :diminish auto-revert-mode
  :init
  (general-define-key :prefix my-leader
		              "g" 'magit-status)
  (general-define-key :keymaps 'smerge-mode-map
                      :states 'normal
                      "n" #'smerge-next
                      "p" #'smerge-prev
                      "M-l" #'smerge-keep-lower
                      "M-u" #'smerge-keep-upper))

(use-package ivy
  :ensure t
  :ensure smex
  :ensure counsel
  :diminish ivy-mode
  :preface
  (defun counsel-rg-dwim ()
    (interactive)
    (if (equal (projectile-project-name) "-")
	(counsel-rg)
      (counsel-projectile-rg)))
  :bind (("M-x" . counsel-M-x)
	 :map ivy-mode-map
	 ("<escape>" . minibuffer-keyboard-quit))
  :init
  (setq projectile-switch-project-action #'counsel-projectile-find-file
	projectile-completion-system 'ivy
	ivy-height 15
	ivy-fixed-height-minibuffer t
	ivy-use-virtual-buffers t
	ivy-format-function #'ivy-format-function-line
	ivy-count-format "(%d/%d) "
	ivy-display-style 'fancy)
  (general-define-key :prefix my-leader
   "f" 'counsel-find-file
   "b" 'ivy-switch-buffer)
  (if (executable-find "rg")
      (general-define-key :prefix my-leader "pg" #'counsel-rg-dwim)
    (general-define-key :prefix my-leader "pg" 'counsel-git-grep))
  (ivy-mode 1))

(use-package hl-line
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'html-mode-hook #'hl-line-mode)
  :config
  (defadvice hl-line-highlight (around ignore-remote first activate)
    ad-do-it
    (when (save-excursion
              (forward-line)
              (eobp))
      (hl-line-unhighlight)))
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
  :init (general-define-key :prefix my-leader "T" 'transpose-frame))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; C/C++ SETTINGS
(use-package c++-mode
  :mode (("\\.h\\'" . c++-mode))
  :init
  (setq c-basic-offset 4
	gdb-many-windows t
	c-default-style "bsd"))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package cmake-ide
  :ensure t
  :config (cmake-ide-setup))

(use-package rtags
  :ensure t
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
		      "q" 'kill-current-buffer))


(use-package irony
  :ensure t
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
  (add-hook 'irony-mode-hook #'my/irony-mode-hook)
  :config
  (add-hook 'kill-buffer-hook #'my/irony-cleanup))



(use-package company-irony
  :ensure t
  :init
  (with-eval-after-load 'irony
    (add-to-list 'company-backends 'company-irony)))

(use-package flycheck-irony
  :ensure t
  :init
  (with-eval-after-load 'irony
    (flycheck-irony-setup)))

(use-package company-irony-c-headers
  :ensure t
  :init
  (with-eval-after-load 'irony
    (add-to-list 'company-backends 'company-irony-c-headers)))

(use-package irony-eldoc
  :ensure t
  :init
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package glsl-mode
  :ensure t
  :mode ("\\.[fv]s\\'" . glsl-mode))

(use-package company-jedi
  :ensure t
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
  (when (executable-find "rustc")
    (setenv "LD_LIBRARY_PATH" (concat (f-join (s-trim-right (shell-command-to-string "rustc --print sysroot")) "lib")
                                      ":"
                                      (getenv "LD_LIBRARY_PATH"))))

  (add-hook 'rust-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'rust-enable-format-on-save))


;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (require 'lsp-flycheck)
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states '(normal insert)
;; 		      "C-." 'company-complete)
;;   (general-define-key :keymaps 'rust-mode-map
;; 		      :states 'normal
;; 		      :prefix my-leader
;; 		      "R" 'lsp-rename))


;; (use-package lsp-rust
;;   :ensure t
;;   :config
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (when (executable-find "rustc")
;;     (setenv "RUST_SRC_PATH" (concat (f-join
;; 				     (s-trim-right (shell-command-to-string "rustc --print sysroot"))
;; 				     "lib/rustlib/src/rust/src/"))))
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends #'company-lsp))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda () (setq-local eldoc-documentation-function #'racer-eldoc)))
  (general-define-key :keymaps 'rust-mode-map
		      :states 'normal
		      "M-," #'pop-tag-mark
		      "M-." #'racer-find-definition))

(use-package company-racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda () (add-to-list 'company-backends 'company-racer))))

(use-package flycheck-rust
    :ensure t
    :init
    (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;; Web development
(use-package web-mode
  :ensure t
  :mode (("\\.ts[x]?\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)))

(use-package typescript-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'html-mode-hook #'emmet-mode)
  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'js2-jsx-mode-hook #'emmet-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-strict-missing-semi-warning nil)
  (add-hook 'js2-jsx-mode-hook (lambda () (flycheck-mode -1))))

(use-package tide
  :ensure t
  :preface
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))
  :init
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (s-matches-p (rx "ts" (zero-or-one "x"))
				 (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (general-define-key :keymaps '(web-mode-map typescript-mode-map)
		      :states 'normal
		      "M-." #'tide-jump-to-definition
		      "M-," #'tide-jump-back
		      "M--" #'tide-references)
  (general-define-key :prefix my-leader
		      :keymaps '(web-mode-map typescript-mode-map)
		      :states 'normal
		      "R" #'tide-rename-symbol)
  :config
  (add-hook 'before-save-hook #'tide-format-before-save))

(use-package tern
  :ensure t
  :ensure company-tern
  :diminish tern-mode
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
  :defer t
  :init
  (general-define-key :keymaps 'org-mode-map
		      :states '(normal insert)
		      "M-r" #'org-ref-helm-insert-cite-link)
  :config
  (require 'doi-utils))

(use-package darkroom
  :ensure t
  :config
  (setq darkroom-margins 0.25
	darkroom-text-scale-increase 1))

(use-package tramp
  :config
  (setq tramp-verbose 2))

(use-package backup-walker
  :ensure t
  :commands backup-walker-start
  :init (add-to-list 'evil-emacs-state-modes 'backup-walker-mode))

(use-package neotree
  :ensure t
  :config
  (general-define-key :keymaps 'neotree-mode-map
		      :states 'normal
		      "<return>" #'neotree-enter
		      "<tab>" #'neotree-enter))

(use-package refine
  :ensure t
  :config
  (general-define-key :keymaps 'refine-mode-map
                      :states 'normal
                      "D" #'refine-delete))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (general-define-key :keymaps 'pdf-view-mode-map
		      :states 'normal
		      "C-h" #'windmove-left
		      "C-j" #'windmove-down
		      "C-k" #'windmove-up
		      "C-l" #'windmove-right
		      "j" #'pdf-view-next-line-or-next-page
		      "k" #'pdf-view-prveious-line-or-previous-page
		      "M-j" #'pdf-view-next-page
		      "M-k" #'pdf-view-previous-page))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

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
