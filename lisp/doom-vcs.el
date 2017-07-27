;;; doom-vcs.el --- version control awareness

(eval-when-compile
  (require 'use-package))
(setq shackle-rules
        `(;; Util
          ("^\\*.+-Profiler-Report .+\\*$" :align below :size 0.3 :regexp t)
          ("*esup*"            :align below :size 0.4 :noselect t)
          ("*minor-modes*"     :align below :size 0.5 :noselect t)
          ("*eval*"            :align below :size 16  :noselect t)
          ;; Doom
          (" *doom*"           :align below :size 35  :select t)
          ("^\\*doom:.+\\*$"   :align below :size 35  :select t :regexp t)
          ("^\\*doom.+\\*$"    :align below :size 12  :noselect t :regexp t)
          ;; Emacs
          ("*Pp Eval Output*"  :align below :size 0.3)
          ("*Apropos*"         :align below :size 0.3)
          ("*Backtrace*"       :align below :size 25  :noselect t)
          ("*Help*"            :align below :size 16  :select t)
          ("*Messages*"        :align below :size 15  :select t)
          ("*Warnings*"        :align below :size 10  :noselect t)
          (compilation-mode    :align below :size 15  :noselect t)
          (eww-mode            :align below :size 30  :select t)
          ("*command-log*"     :align right :size 28  :noselect t)
          ;; vcs
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          (vc-annotate-mode    :same t)))



(use-package git-gutter
  :ensure t
  :ensure git-gutter-fringe
  :commands (git-gutter-mode doom/vcs-next-hunk doom/vcs-prev-hunk
             doom/vcs-show-hunk doom/vcs-stage-hunk doom/vcs-revert-hunk)
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config
  (require 'git-gutter-fringe)
  (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)

  ;; NOTE If you want the git gutter to be on the outside of the margins (rather
  ;; than inside), `fringes-outside-margins' should be non-nil.

  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center)

  ;; Refreshing git-gutter
  (advice-add 'evil-force-normal-state :after 'git-gutter)
  (add-hook 'focus-in-hook 'git-gutter:update-all-windows)

  (defalias 'doom/vcs-next-hunk    'git-gutter:next-hunk)
  (defalias 'doom/vcs-prev-hunk    'git-gutter:previous-hunk)
  (defalias 'doom/vcs-show-hunk    'git-gutter:popup-hunk)
  (defalias 'doom/vcs-stage-hunk   'git-gutter:stage-hunk)
  (defalias 'doom/vcs-revert-hunk  'git-gutter:revert-hunk))

(after! vc-annotate
  (evil-set-initial-state 'vc-annotate-mode     'normal)
  (evil-set-initial-state 'vc-git-log-view-mode 'normal))

(use-package browse-at-remote
  :commands (browse-at-remote/browse browse-at-remote/get-url))

;; Ediff
(defvar doom-ediff-enabled nil)
(add-hook! ediff-load
  (setq ediff-diff-options           "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain) ; no extra frames

  ;; Brighten other buffers
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

  ;; TODO Custom modeline for ediff buffers

  ;; For modeline awareness
  (add-hook! ediff-startup (setq doom-ediff-enabled t))
  (add-hook! ediff-quit    (setq doom-ediff-enabled nil)))

(provide 'doom-vcs)
;;; doom-vcs.el ends here
