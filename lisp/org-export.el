(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'org)
(when (package-installed-p 'org-ref)
  (require 'org-ref))
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
(setq org-latex-prefer-user-labels t)
(setq org-export-async-debug t)
(setq org-latex-pdf-process
      '("xelatex -bibtex -shell-escape -interaction nonstopmode %f"
	"bibtex %b"
	"xelatex -bibtex -shell-escape -interaction nonstopmode %f"
	"xelatex -bibtex -shell-escape -interaction nonstopmode %f"))

(setq org-latex-listings t
      org-export-with-sub-superscripts nil
      org-export-with-smart-quotes t
      org-src-preserve-indentation t)

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-default-packages-alist
	     '("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	       "hyperref" nil)
	     t)
