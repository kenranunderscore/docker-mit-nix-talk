(require 'org)
(require 'org-re-reveal)
(require 'htmlize)

(org-babel-do-load-languages
 'org-babel-load-languages '((plantuml . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-exec-mode 'plantuml)

;; Syntax highlighting via CSS
(setq org-html-htmlize-output-type 'css)

(defun org-re-reveal-export-file (filename &optional reveal-root reveal-mathjax-url)
  "Export FILENAME to HTML.  Optionally change or set the path to a
reveal.js checkout and a MathJax checkout."
  (interactive "f")
  (let* ((buffer (find-file-noselect filename))
         (org-re-reveal-root (or reveal-root org-re-reveal-root))
         (org-re-reveal-mathjax-url reveal-mathjax-url))
    (message (concat "Exporting " filename " to HTML"))
    (with-current-buffer buffer
      (org-re-reveal-export-to-html))))
