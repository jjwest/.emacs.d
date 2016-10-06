
;;; code:

(deftheme airline-base16-gui-dark
  "Designed for use with the base16 emacs color schemes in the gui

url: https://github.com/mkaito/base16-emacs")

(let ((normal-outer-foreground  (face-background 'highlight))          (normal-outer-background  (face-foreground 'link))
      (normal-inner-foreground  (face-foreground 'font-lock-doc-face)) (normal-inner-background  "#35393B")
      (normal-center-foreground (face-foreground 'font-lock-doc-face)) (normal-center-background "#35393B")

      (insert-outer-foreground  (face-background 'highlight))          (insert-outer-background  (face-foreground 'success))
      (insert-inner-foreground  (face-foreground 'success))            (insert-inner-background  "#35393B")
      (insert-center-foreground (face-foreground 'font-lock-doc-face)) (insert-center-background "#35393B")

      (visual-outer-foreground  (face-background 'highlight))          (visual-outer-background  (face-foreground 'warning))
      (visual-inner-foreground  (face-foreground 'warning))            (visual-inner-background  "#35393B")
      (visual-center-foreground (face-foreground 'font-lock-doc-face)) (visual-center-background "#35393B")

      (replace-outer-foreground (face-background 'highlight)) (replace-outer-background (face-foreground 'error))
      (emacs-outer-foreground   (face-background 'highlight)) (emacs-outer-background   (face-foreground 'link-visited))

      (inactive1-foreground (face-foreground 'font-lock-doc-face)) (inactive1-background "#35393B")
      (inactive2-foreground (face-foreground 'font-lock-doc-face)) (inactive2-background "#35393B"))

  (airline-themes-set-deftheme 'airline-base16-gui-dark)

  (when airline-cursor-colors
    (progn
     (setq evil-emacs-state-cursor   emacs-outer-background)
     (setq evil-normal-state-cursor  normal-outer-background)
     (setq evil-insert-state-cursor  `(bar ,insert-outer-background))
     (setq evil-replace-state-cursor replace-outer-background)
     (setq evil-visual-state-cursor  visual-outer-background)))
)

(airline-themes-set-modeline)

(provide-theme 'airline-base16-gui-dark)
;;; airline-base16-gui-dark-theme.el ends here
