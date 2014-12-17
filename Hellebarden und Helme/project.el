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
