;; Copyright (C) 2017  Alex Schroeder <alex@gnu.org>
;; 
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;; 
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This code parses the LaTeX file and posts every section a page on
;; the Halberds & Helments.

;;; Code:

(require 'oddmuse-curl)

(defvar publish-wiki "Halberds and Helmets")

(assert (assoc publish-wiki oddmuse-wikis))

(defun publish-section ()
  (interactive)
  (let* ((end (save-excursion
		(re-search-forward "^\\\\\\(section\\|chapter\\)")
		(line-beginning-position)))
	 (start (save-excursion
		  (re-search-backward "^\\\\\\(section\\|chapter\\){\\(.*\\)}")
		  (line-beginning-position)))
	 (text (buffer-substring-no-properties start end))
	 (title (match-string 2)))
    (with-current-buffer (get-buffer-create "*Oddmuse Publish")
      (erase-buffer)
      (insert text)
      (latex-to-oddmuse (point-min) (point-max))
      (skip-syntax-backward "-")
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (skip-syntax-forward "-")
      (delete-region (point-min) (point))
      (insert (format "[[image/right/twenty:Image 1 for %s|%s]]\n"
		      title title))
      (display-buffer (current-buffer))
      (oddmuse-mode)
      (setq oddmuse-wiki publish-wiki
	    oddmuse-page-name title))))
