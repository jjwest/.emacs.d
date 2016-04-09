(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; The essentials
(delete-selection-mode 1)
(setq initial-major-mode 'text-mode
      auto-save-default nil
      make-backup-files nil
      custom-safe-themes t
      scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)
(global-font-lock-mode 1) 
(electric-pair-mode 1)

(use-package smartparens
	     :ensure t
	     :config (show-smartparens-global-mode 1))

(use-package gruvbox-theme :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode)
  (with-eval-after-load 'undo-tree (diminish 'undo-tree-mode))
  (with-eval-after-load  'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'abbrev (diminish 'abbrev-mode)))

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-pos-tip
    :ensure t
    :config (flycheck-pos-tip-mode)))

;; C/C++ STYLE SETTINGS
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "bsd") 
(setq-default c-basic-offset 4)
(setq-default gdb-many-windows t)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; PYTHON SETTINGS
(use-package company-jedi
  :ensure t
  :config (add-to-list 'company-backends 'company-jedi))

(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode
  :init (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package magit
  :ensure t
  :diminish auto-revert-mode)

(use-package evil-leader
  :ensure t
  :diminish evil-leader-mode
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
  "f" 'find-file
  "b" 'switch-to-buffer
  "B" 'buffer-menu
  "k" 'kill-buffer
  "pp" 'projectile-switch-project
  "pf" 'projectile-find-file
  "pk" 'projectile-kill-buffers
  "ss" 'split-window-horizontally
  "vv" 'split-window-vertically
  "dw"  'delete-window
  "do" 'delete-other-windows
  "sf" 'save-buffer
  "sa" 'save-some-buffers
  "g" 'magit-status
  "pt" 'projectile-find-other-file
  "qr" 'ggtags-query-replace
  "r" 'replace-string
  "x" 'ansi-term
  "jd" 'ggtags-find-definition
  "D" 'dired
  "c" 'idomenu))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

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
    :config (ido-ubiquitous-mode 1)))

(use-package idomenu :ensure t)

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's asynchronous function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(use-package irony
  :ensure t
  :defer t
  :diminish irony-mode
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'eldoc-mode)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :config
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
  (add-hook 'text-mode-hook 'linum-relative-mode)
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
(global-set-key [escape] 'evil-exit-emacs-state)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "chartreuse" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(company-clang-executable "/usr/bin/clang-3.7")
 '(company-idle-delay 0)
 '(company-show-numbers nil)
 '(company-tooltip-flip-when-above t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("5d5bc275d76f782fcb4ca2f3394031f4a491820fd648aed5c51efc15d472562e" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "cbfd0a579def88d8d8ff6ebe47771430afb4f2fa49697bcfff6725d1eba6e4f5" "7a300294c75439710241e5e607b1dafa60b66b7c3a0494b791bfa683cf45bfba" "e620d5162da528134c18de0da3b207f8051e5de39bf13068d1035a88cd2da780" "c95a117efd4eeefc410aa55ea883f74f07ffd64a887538e629a6c7f845fa6add" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "7742fa3bcce36bc9b82eb4bc327754472af0618b0d6e0bdcdec0145467af76b1" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" "b28e65eff6d3d3dcb6e59c8e4cfb12daf560b8749cf70bb89a55c492b6b9787f" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "ad8f255143ab71ba24070246906f6b758f9a2a7f5baed4dc1ae14b9a2f4753f4" "eaf07d1628bc01547cb79d2879e8a0d776cc802aaab7ff57b9d06a9a807d4ba8" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "6c0a087a4f49c04d4002393ffd149672f70e4ab38d69bbe8b39059b61682b61c" "405b0ac2ac4667c5dab77b36e3dd87a603ea4717914e30fcf334983f79cfd87e" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "519d1b3cb7345cc9be10b4b0489436ae2d1b0690470d8d78f8e4e1ff19b83a86" default)))
 '(display-time-mode t)
 '(fci-rule-color "#3E3D31")
 '(flycheck-c/c++-gcc-executable "gcc-5")
 '(flycheck-gcc-language-standard "c++14")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message "")
 '(irony-additional-clang-options (quote ("-std=c++14")))
 '(linum-format " %1i ")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(projectile-other-file-alist
   (quote
    (("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cpp" "cc" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "hh" "h")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" ""))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-enable-auto-expanding t)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

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

(provide 'init)
