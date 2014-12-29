;; this requires org-mode as distributed with Emacs 24.4
(let ((dir (file-name-directory (buffer-file-name))))
  (setq org-publish-project-alist
	`(("Hellebarden und Helme"
	   :base-directory ,dir
	   :publishing-directory ,dir
	   :publishing-function org-html-publish-to-html
	   :exclude "-source\\.org"
	   :section-numbers nil
	   :html-postamble nil
	   :makeindex t))))
(org-publish-project "Hellebarden und Helme" t)
