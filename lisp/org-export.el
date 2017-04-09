(require 'ox)
(require 'ox-latex)

(add-to-list 'org-latex-classes
	     '("koma-article"
	       "\\documentclass{scrartcl}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-export-async-debug nil)
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
	"xelatex -interaction nonstopmode %f"))
(setq org-latex-listings 'listings
      org-export-with-sub-superscripts nil
      org-export-with-smart-quotes t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
