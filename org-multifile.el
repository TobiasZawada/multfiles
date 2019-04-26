;;; org-multifile.el --- Combine multiple org files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tobias Zawada

;; Author: Tobias Zawada <naehring@smtp.1und1.de>
;; Keywords: outlines, files
;; Version: 0
;; URL: https://github.com/TobiasZawada/multfiles
;; Package-Requires: ((emacs "25.1") (multfiles "0") (elgrep "1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'multfiles)

(defun org-multifile (re &optional dir file-name-re)
  "Collect org-entries matching regular expression RE in one multi-file-buffer."
  (interactive (list
		(read-regexp "Regular expression for header line:")
		(read-directory-name "Directory:")
		(read-regexp "Regular expression for org-files:" "^20.*\.org$")))
  (require 'elgrep)
  (unless file-name-re (setq file-name-re "\\.org$"))
  (cl-loop for filematch in (elgrep dir file-name-re (concat "^\\*.*" re)
				    :c-op (lambda (beg _end)
					    (cons beg
						  (save-excursion
						    (save-match-data
						      (let (at-head)
							(while (and (setq at-head (re-search-forward "^* " nil 'noErr))
								    (org-in-block-p '("src"))))
							(if at-head (line-end-position 0)
							  (point))
							)))))
				    :abs t)
	   do
	   (with-current-buffer (find-file (car filematch))
	     (cl-loop for matchdata in (cdr filematch) do
		      (let* ((context (plist-get (car matchdata) :context))
			     (b (car context))
			     (e (cdr context)))
			(mf/mirror-region-in-multifile b e (concat "*mf:" re) "* [[file:" "]]"))))))

(provide 'org-multifile)
;;; org-multifile.el ends here
