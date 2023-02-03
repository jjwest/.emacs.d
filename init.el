;;; init.el --- My Emacs config --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Nothing here
;;; Code:

;; Better garbage collection settings

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 27)
  (package-initialize))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun inhibit-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun resume-gc ()
  (setq gc-cons-threshold gcmh-high-cons-threshold))

(add-hook 'minibuffer-setup-hook #'inhibit-gc)
(add-hook 'minibuffer-exit-hook #'resume-gc)

(require 'use-package)

(use-package gcmh
  :ensure t
  :config
  (setq gcmh-idle-delay 10)
  (setq gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-mode))


;; General settings and better defaults
(setq custom-safe-themes t
      custom--inhibit-theme-enable nil
      inhibit-startup-screen t
      initial-scratch-message ""
      initial-major-mode 'text-mode
      load-prefer-newer t
      ad-redefinition-action 'accept
      uniquify-buffer-name-style 'forward
      message-log-max 200
      bidi-paragraph-direction 'left-to-right
      auto-revert-check-vc-info t
      show-paren-delay 0
      save-interprogram-paste-before-kill t
      make-backup-files nil
      select-enable-clipboard t
      display-time-24hr-format t
      display-time-day-and-date t
      display-time-default-load-average nil
      auto-window-vscroll nil
      exec-path-from-shell-check-startup-files nil
      read-process-output-max (* 3 1024 1024)
      locale-coding-system 'utf-8)

(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil     ; hide cursors in other windows
 display-line-numbers-width 3
 frame-inhibit-implied-resize t
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil     ; disable mode-line mouseovers
 mouse-yank-at-point t               ; middle-click paste at point, not at click
 ibuffer-use-other-window t
 resize-mini-windows 'grow-only         ; Minibuffer resizing
 show-help-function nil                 ; hide :help-echo text
 split-width-threshold 160              ; favor horizontal splits
 uniquify-buffer-name-style 'forward
 use-dialog-box nil                     ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 tab-width 4
 require-final-newline nil
 split-height-threshold nil
 indent-tabs-mode nil)

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
(load-file custom-file)

(defun set-font-on-start (frame)
  (select-frame frame)
  (when (member "Office Code Pro" (font-family-list))
    (set-frame-font "Office Code Pro Medium-10" t t))
  (remove-hook 'after-make-frame-functions 'set-font-on-start))

;; Set font
(if (daemonp)
    (add-hook 'after-make-frame-functions #'set-font-on-start)
  (when (member "Office Code Pro" (font-family-list))
    (set-frame-font "Office Code Pro Medium-10" t t)))

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
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (>= emacs-major-version 27)
  (global-so-long-mode t))

(when (>= emacs-major-version 26)
  (add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative))))

(when (executable-find "sccache")
  (let ((env-vars initial-environment)
        (sccache-wrapper (concat "RUSTC_WRAPPER=" (executable-find "sccache"))))
    (add-to-list 'env-vars sccache-wrapper)
    (setq compilation-environment env-vars)))

(defun maybe-kill-buffers (frame)
  "Kill all live buffers when the last frame is closed."
  (when (<= (length (frame-list)) 2)
    (mapc #'kill-buffer (buffer-list))
    (cd (expand-file-name "~/"))))

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

(add-hook 'kill-buffer-hook #'my/dont-kill-scratch)

;; Always give new frames focus
(when (daemonp)
  (add-hook 'after-make-frame-functions
	        (lambda (frame)
	          (select-frame-set-input-focus frame))))

(defun my/save-all-buffers ()
  "Save all buffers without prompt."
  (interactive)
  (save-some-buffers t))

(defcustom delete-trailing-whitespace-on-save t
  "Sets if trailing whitespace should be deleted one saving file")

(defun my/maybe-delete-trailing-whitespace ()
  (when delete-trailing-whitespace-on-save
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my/maybe-delete-trailing-whitespace)

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

(defvar current-brace-style 'same-line
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
    (insert " {")))

(defun change-theme (theme)
  "Change theme to THEME, disabling all current themes first."
  (interactive
   (list
    (intern (completing-read "Change to theme: "
			                 (mapcar 'symbol-name
				                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme))

(defun maybe-insert-elisp-header ()
  "Insert lexical scope header in new emacs-lisp files"
  (when (and (= (buffer-size) 0)
             (not (equal (buffer-name) "*scratch*")))
    (insert ";;; -*- lexical-binding: t -*-\n")))

(add-hook 'emacs-lisp-mode-hook #'maybe-insert-elisp-header)

;; Packages
(use-package general
  :ensure t
  :defines my-leader
  :config
  (defconst my-leader ",")
  (general-define-key :prefix my-leader
		              :keymaps '(normal visual)
		              "n" #'narrow-or-widen-dwim)
  (general-define-key :prefix my-leader
                      :keymaps'normal
		              "dw" #'delete-window
		              "do" #'delete-other-windows
		              "sf" #'save-buffer
		              "sa" #'my/save-all-buffers
		              "k" #'kill-current-buffer
		              "B" #'ibuffer
                      "c" #'compile
		              "P" #'proced
		              "W" #'winner-undo
		              "ss" #'my/split-window-horizontal
		              "sv" #'my/split-window-vertical))


(use-package solaire-mode
  :ensure t
  :config
  (setq solaire-mode-remap-line-numbers t)
  (defcustom use-solaire-mode t
    "Use solaire mode"
    :type 'boolean)

  (defun maybe-use-solaire ()
    (when use-solaire-mode
      (turn-on-solaire-mode)))

  (solaire-global-mode +1))

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
      (custom-theme-set-faces
       'doom-one
       `(font-lock-preprocessor-face ((t (:foreground "#DA8548" :bold t))))
       `(line-number ((t (:foreground "#5B6268"))))
       `(font-lock-variable-name-face ((t (:foreground "#DFDFDF")))))))

  (defun load-doom-theme (frame)
    (select-frame frame)
    (load-theme 'doom-one)
    (remove-hook 'after-make-frame-functions 'load-doom-theme))
  :init
  (advice-add #'load-theme :after #'tweak-doom-theme)
  (advice-add #'change-theme :after #'tweak-doom-theme)
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'load-doom-theme)
    (load-theme 'doom-one)))

(use-package evil-anzu
  :ensure t)

(use-package doom-modeline
  :load-path "~/.emacs.d/lisp"
  :config
  (defun +doom-modeline|init ()
    "Set the default modeline."
    (doom-set-modeline 'main t)
    (with-current-buffer "*scratch*"
      (doom-set-modeline 'main)))
  (with-current-buffer "*Messages*"
    (doom-set-modeline 'main))
  (add-hook 'after-init-hook #'+doom-modeline|init))


(use-package undo-fu
  :ensure t)

(use-package evil
  :ensure t
  :config
  (general-define-key :states 'normal
                      "j" #'evil-next-visual-line
                      "k" #'evil-previous-visual-line
                      "TAB" #'indent-for-tab-command
                      "C-h" #'evil-window-left
                      "C-j" #'evil-window-down
                      "C-k" #'evil-window-up
                      "C-l" #'evil-window-right
                      "M-j" #'evil-scroll-down
                      "M-k" #'evil-scroll-up
                      "C-a" #'beginning-of-line
                      "C-q" #'evil-scroll-line-up
                      "C-e" #'evil-scroll-line-down
                      "S" #'my/split-line
                      "u" #'undo-fu-only-undo
                      "U" #'undo-fu-only-redo
                      "M-." #'xref-find-definitions
                      "M-C-." #'xref-find-definitions-other-window
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
  (general-define-key :keymaps 'xref--button-map
                      "RET" #'xref-goto-xref
                      "q" #'quit-window)
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


(use-package evil-numbers
  :ensure t
  :config
  (general-define-key :states 'normal
                      "M-<up>" #'evil-numbers/inc-at-pt
                      "M-<down>" #'evil-numbers/dec-at-pt))

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
  :bind (:map company-active-map
	          ("<C-return>" . my/company-abort-and-newline)
              ("<tab>" . 'yas-next-field-or-maybe-expand)
              ("M-n" . 'company-select-next-or-abort)
              ("M-p" . 'company-select-previous-or-abort))
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (general-define-key "C-SPC" 'company-complete)
  (general-define-key :states '(normal insert)
		              "C-SPC" 'company-complete)
  (setq company-idle-delay 0.0
	    company-tooltip-align-annotations t
	    company-dabbrev-ignore-case nil
	    company-dabbrev-downcase nil
        company-format-margin-function nil
	    company-require-match nil))


(use-package counsel-projectile
  :ensure t
  :diminish projectile-mode
  :init
  (general-define-key :prefix my-leader
                      :states 'normal
  		              "pp" 'counsel-projectile-switch-project
  		              "pf" 'counsel-projectile-find-file
  		              "pd" 'counsel-projectile-find-dir
  		              "pk" 'projectile-kill-buffers
  		              "t" 'projectile-find-other-file
		              "T" 'projectile-find-other-file-other-window)

  (setq my/projectile-ignored-project-directories '(".rustup" ".cargo"))
  (defun my/projectile-ignore-project-function (file)
    (let ((result nil))
            (dolist (ignored-dir my/projectile-ignored-project-directories)
              (if (string-search ignored-dir file)
                  (setq result t)))
            result))

  (setq projectile-enable-caching t
        projectile-ignored-project-function #'my/projectile-ignore-project-function)

  (setq projectile-other-file-alist
        '(("cpp" "h" "hpp" "ipp" "hh")
          ("ipp" "h" "hpp" "cpp")
          ("hpp" "h" "ipp" "cpp" "cc")
          ("cxx" "h" "hxx" "ixx")
          ("ixx" "h" "hxx" "cxx")
          ("hxx" "h" "ixx" "cxx")
          ("c" "h")
          ("m" "h")
          ("mm" "h")
          ("h" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
          ("cc" "h" "hh" "hpp")
          ("hh" "cc" "cpp")
          ("vert" "frag")
          ("frag" "vert")
          (nil "lock" "gpg")
          ("lock" "")
          ("gpg" "")))

  ;; Don't slow Emacs to a crawl when working with TRAMP.
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory)
      ad-do-it))
  :config
  (projectile-mode))


(use-package terminal-here
  :ensure t
  :config
  (when (executable-find "alacritty")
    (setq terminal-here-linux-terminal-command 'alacritty))
  (general-define-key :prefix my-leader :states 'normal "x" #'terminal-here))

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
  (general-define-key :prefix my-leader :states 'normal "r" 'iedit-dwim))

(use-package rg
  :ensure t
  :config
  (rg-define-search my/rg-search-project
    :query ask
    :dir project
    :files "all")

  (rg-define-search my/rg-search-dir
    :query ask
    :dir current
    :files "all")

  (defun my/rg-search (arg)
    (interactive "P")
    (if arg
        (call-interactively #'my/rg-search-project)
      (call-interactively #'my/rg-search-dir)))

  (general-define-key :prefix my-leader
                      :states 'normal
                      "g" #'my/rg-search))

(use-package wgrep
  :ensure t
  :init
  (general-define-key :prefix my-leader
		              :keymaps 'grep-mode-map
		              :states 'normal
		              "w" 'wgrep-change-to-wgrep-mode))

(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook #'eldoc-mode))

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
  :load-path "~/.emacs.d/lisp"
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
  (setq magit-section-visibility-indicator nil)
  (general-define-key :prefix my-leader
                      :states 'normal
		              "G" 'magit-status)
  (general-define-key :keymaps 'smerge-mode-map
                      :states 'normal
                      "n" #'smerge-next
                      "p" #'smerge-prev
                      "M-l" #'smerge-keep-lower
                      "M-u" #'smerge-keep-upper)
  (require 'transient)
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq) )

(use-package smerge-mode
  :ensure hydra
  :config
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (smerge-hydra/body)))))


(use-package hungry-delete
  :ensure t
  :config
  (general-define-key :keymaps 'prog-mode-map
                      :states 'insert
                      "<backspace>" #'hungry-delete-backward))

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
	    ivy-format-functions-alist '((t . ivy-format-function-line))
	    ivy-count-format "(%d/%d) ")

  (general-define-key :prefix my-leader
                      :states 'normal
                      "f" 'counsel-find-file
                      "b" 'ivy-switch-buffer
                      "v" #'counsel-imenu)
  (if (executable-find "rg")
      (general-define-key :prefix my-leader :states 'normal "pg" #'counsel-rg-dwim)
    (general-define-key :prefix my-leader :states 'normal "pg" 'counsel-git-grep))
  (ivy-mode 1))

(use-package hl-line
  :init
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'html-mode-hook #'hl-line-mode)
  :config
  (defun hl-line-range ()
    (cons (line-beginning-position)
          (cond ((save-excursion
                   (goto-char (line-end-position))
                   (and (eobp) (not (bolp))))
                 (1- (line-end-position)))
                ((or (eobp) (save-excursion (forward-line) (eobp)))
                 (line-end-position))
                (t
                 (line-beginning-position 2)))))
  (setq hl-line-range-function #'hl-line-range)
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
  :init (general-define-key :prefix my-leader :states 'normal "wt" 'transpose-frame))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; C/C++ SETTINGS
(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode))
  :init
  (setq c-basic-offset 4
	    gdb-many-windows t
	    c-default-style "bsd")
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inextern-lang 0))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))


(use-package ccls
  :ensure t
  :after lsp-mode
  :config
  (general-define-key :keymaps '(c-mode-map c++-mode-map)
                      :states 'normal
                      "M--" #'lsp-find-references))

(use-package glsl-mode
  :ensure t
  :mode ("\\.[fv]s\\'" . glsl-mode))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; RUST SETTINGS
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-match-angle-brackets nil
        rust-format-on-save t)
  (add-hook 'rust-mode-hook #'eldoc-mode))

(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-rust)
  (setq lsp-eldoc-hook '(lsp-hover)
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate nil
        lsp-enable-links nil
        lsp-keep-workspace-alive nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-diagnostic-package :none
        lsp-enable-file-watchers nil
        lsp-lens-enable nil
        lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-call-info-full nil
        lsp-rust-analyzer-completion-postfix-enable nil)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (general-define-key :keymaps '(rust-mode-map c-mode-map c++-mode-map)
		              :states 'normal
		              :prefix my-leader
		              "R" 'lsp-rename))


(use-package typescript-mode
  :ensure t)

(use-package tramp
  :config
  (setq tramp-verbose 2))

(use-package neotree
  :ensure t
  :config
  (general-define-key :keymaps 'neotree-mode-map
		              :states 'normal
		              "<return>" #'neotree-enter
		              "<tab>" #'neotree-enter))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(defun my/new-tab ()
  (interactive)
  (tab-bar-mode 1)
  (tab-bar-new-tab))

(defun my/close-tab ()
  (interactive)
  (tab-bar-close-tab)
  (when (eq 1 (length (tab-bar-tabs)))
    (tab-bar-mode -1)))

(general-define-key :keymaps 'normal "M-1" (lambda () (interactive) (tab-bar-select-tab 1)))
(general-define-key :keymaps 'normal "M-2" (lambda () (interactive) (tab-bar-select-tab 2)))
(general-define-key :keymaps 'normal "M-3" (lambda () (interactive) (tab-bar-select-tab 3)))
(general-define-key :keymaps 'normal "M-r" (lambda () (interactive) (call-interactively #'tab-bar-rename-tab)))
(general-define-key :keymaps 'normal "C-t" #'my/new-tab)
(general-define-key :keymaps 'normal "C-w" #'my/close-tab)


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
