;; do need the latest org-mode for @<br/> to work?
(add-to-list 'load-path "~/src/org-mode/lisp")
(add-to-list 'Info-default-directory-list "~/src/org-mode/doc")
(let ((dir (file-name-directory (buffer-file-name))))
  (setq org-publish-project-alist
	`(("Hellebarden und Helme"
	   :base-directory ,dir
	   :publishing-directory ,dir
	   :exclude "-source\\.org"
	   :section-numbers nil
	   :html-postamble nil
	   :makeindex t))))
(org-publish-project "Hellebarden und Helme" t)
