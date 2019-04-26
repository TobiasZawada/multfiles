;;; multfiles.el --- View and edit parts of multiple files in one buffer

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com> modified by Tobias Zawada
;; Keywords: multiple files
;; Version: 0
;; URL: https://github.com/TobiasZawada/multfiles
;; Package-Requires: ((emacs "25.1") (elgrep "1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bind a key to `mf/mirror-region-in-multifile`, let's say `C-!`. Now
;; mark a part of the buffer and press it. A new \*multifile\* buffer pops
;; up. Mark some other part of another file, and press `C-!` again. This
;; is added to the \*multifile\*.

;; You can now edit the \*multifile\* buffer, and watch the original files change.
;; Or you can edit the original files and watch the \*multifile\* buffer change.

;; **Warning** This API and functionality is highly volatile.
;;
;; Problems:
;;
;; (1) multfiles.el not working together with undo-tree-mode.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defun mf/mirror-region-in-multifile (beg end &optional multifile-buffer head-beg head-end)
  "Add region from BEG to END to MULTIFILE-BUFFER defaulting to \"*multifile*\".
Decorate headings with HEAD-BEG and HEAD-END each defaulting to \"\"."
  (interactive (list (region-beginning) (region-end)
                     (when current-prefix-arg
                       (read-buffer "Mirror into buffer: " "*multifile*"))))
  (deactivate-mark)
  (let ((buffer (current-buffer))
        (mode major-mode))
    (switch-to-buffer-other-window (or multifile-buffer "*multifile*"))
    (funcall mode)
    (multifiles-minor-mode 1)
    (mf--add-mirror buffer beg end head-beg head-end)
    (switch-to-buffer-other-window buffer)))

(defvar multifiles-minor-mode-map nil
  "Keymap for multifiles minor mode.")

(unless multifiles-minor-mode-map
  (setq multifiles-minor-mode-map (make-sparse-keymap)))

(define-key multifiles-minor-mode-map (vector 'remap 'save-buffer) 'mf/save-original-buffers)

(defun mf/save-original-buffers ()
  (interactive)
  (let (any-buffer-saved)
    (cl-loop for it in (mf--original-buffers) do
	  (if (buffer-live-p it)
	      (with-current-buffer it
		(when (and buffer-file-name (buffer-modified-p))
		  (save-buffer)
		  (setq any-buffer-saved t)))
	    (message "Buffer %S not living anymore and not saved." it)
	    ))
    (when (null any-buffer-saved)
      (message "Multfile: No buffer saved."))
    (set-buffer-modified-p nil)))

;; (defun mf--original-buffers ()
;;   (->> (overlays-in (point-min) (point-max))
;;     (--filter (equal 'mf-mirror (overlay-get it 'type)))
;;     (--map (overlay-buffer (overlay-get it 'twin)))
;;     (-distinct)))


(defun mf--original-buffers ()
  (let (l)
    (mapc (lambda (x) (add-to-list 'l x))
	   (mapcar (lambda (it)
		     (overlay-buffer (overlay-get it 'twin)))
		   (cl-loop for  ol in (overlays-in (point-min) (point-max))
			 if (equal 'mf-mirror (overlay-get ol 'type))
			 collect ol)))
    l))

(define-minor-mode multifiles-minor-mode
  "A minor mode for the *multifile* buffer."
  nil "" multifiles-minor-mode-map)

(defun mf--add-mirror (buffer beg end &optional head-beg head-end)
  "Add the piece from BEG to END of BUFFER into the current multifile-buffer.
If HEAD-BEG and HEAD-END are given insert these ones before and after the head line, respectively."
  (unless head-beg (setq head-beg ""))
  (unless head-end (setq head-end ""))
  (let (contents original-overlay mirror-overlay (mf-buffer-modified-p (buffer-modified-p)))
    (mf--add-hook-if-necessary)
    (with-current-buffer buffer
      (mf--add-hook-if-necessary)
      (setq contents (buffer-substring beg end))
      (setq mf-buffer-modified-p (or mf-buffer-modified-p (buffer-modified-p)))
      (setq original-overlay (create-original-overlay beg end)))
    (insert head-beg (buffer-name buffer) head-end)
    (let ((beg-end (mf---insert-contents contents)))
      (setq mirror-overlay (create-mirror-overlay (car beg-end) (cdr beg-end))))
    (overlay-put mirror-overlay 'twin original-overlay)
    (overlay-put original-overlay 'twin mirror-overlay)
    (set-buffer-modified-p mf-buffer-modified-p)))

(defun mf---insert-contents (contents)
  (goto-char (point-max))
  (newline)
  (cons (point)
	(prog2
	    (insert contents)
	    (point)
	  (newline 2))))

;; (defun mf--any-overlays-in-buffer ()
;;   (--any? (memq (overlay-get it 'type) '(mf-original mf-mirror))
;;           (overlays-in (point-min) (point-max))))

(defun mf--any-overlays-in-buffer ()
  (cl-loop for el in (mapcar (lambda (it) (memq (overlay-get it 'type) '(mf-original mf-mirror)))
			  (overlays-in (point-min) (point-max)))
	thereis el))

(defun mf--add-hook-if-necessary ()
  (unless t ;(mf--any-overlays-in-buffer)
    (add-hook 'post-command-hook 'mf--update-twins)))

(defun mf--remove-hook-if-necessary ()
  (unless t ;(mf--any-overlays-in-buffer)
    (remove-hook 'post-command-hook 'mf--update-twins)))

(defun create-original-overlay (beg end)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'type 'mf-original)
    (overlay-put o 'modification-hooks '(mf--on-modification))
    (overlay-put o 'insert-in-front-hooks '(mf--on-modification))
    (overlay-put o 'insert-behind-hooks '(mf--on-modification))
    o))

(defun create-mirror-overlay (beg end)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'type 'mf-mirror)
    (overlay-put o 'line-prefix mf--mirror-indicator)
    (overlay-put o 'modification-hooks '(mf--on-modification))
    (overlay-put o 'insert-in-front-hooks '(mf--on-modification))
    (overlay-put o 'insert-behind-hooks '(mf--on-modification))
    o))

(defvar mf--changed-overlays nil)
(make-variable-buffer-local 'mf--changed-overlays)

(defun mf--on-modification (o after beg end &optional delete-length)
  (when (not after)
    (if (mf---removed-entire-overlay beg end o)
	(mf--remove-mirror o)
      (overlay-put o 'mod-orig-region (cons (- beg (overlay-start o)) (- end (overlay-start o))))
      ))

  (when (and after (overlay-start o))
    (overlay-put o 'mod-mod-region (cons (- beg (overlay-start o)) (- end (overlay-start o))))
    (mf--update-twin o)
    ;(add-to-list 'mf--changed-overlays o)
    ))

(defun mf---removed-entire-overlay (beg end o)
  (and (<= beg (overlay-start o))
       (>= end (overlay-end o))))

;; (defun mf--update-twins ()
;;   (when mf--changed-overlays
;;     (-each mf--changed-overlays 'mf--update-twin)
;;     (setq mf--changed-overlays nil)))

(defun mf--update-twins ()
  (when mf--changed-overlays
    (mapc 'mf--update-twin mf--changed-overlays)
    (setq mf--changed-overlays nil)))

(defun mf--remove-mirror (o)
  (let* ((twin (overlay-get o 'twin))
         (original (if (mf--is-original o) o twin))
         (mirror (if (mf--is-original o) twin o))
         (mirror-beg (overlay-start mirror))
         (mirror-end (overlay-end mirror)))
    (with-current-buffer (overlay-buffer mirror)
      (save-excursion
        (delete-overlay mirror)
        (delete-region mirror-beg mirror-end)
        (goto-char mirror-beg)
        (delete-blank-lines)
        (mf--remove-hook-if-necessary)))
    (delete-overlay original)
    (mf--remove-hook-if-necessary)))

(defun mf--is-original (o)
  (equal 'mf-original (overlay-get o 'type)))

(defmacro mf--with-mod-region (o r b e &rest body)
  "Get beginning B and end E from region R of overlay O and `let'-run REST with these settings."
  (declare (indent 2) (debug (sexp sexp sexp sexp body)))
  `(let (,b ,e)
     (let ((reg (overlay-get ,o ,r)))
       (setq ,b (car-safe reg))
       (setq ,e (cdr-safe reg)))
     (when (and (number-or-marker-p ,b) (number-or-marker-p ,e))
       ,@body
       )))

(defun mf--update-twin (o)
  (let* ((beg-o (overlay-start o))
	 (twin (overlay-get o 'twin))
         (buffer (overlay-buffer twin)))
    (mf--with-mod-region
     o 'mod-mod-region beg-mod end-mod
     (let ((contents (buffer-substring (+ beg-o beg-mod) (+ beg-o end-mod))))
       (mf--with-mod-region
	o 'mod-orig-region beg-orig end-orig
	(when (and buffer (null (and (= beg-mod end-mod) (= beg-orig end-orig))))
	  (with-current-buffer buffer
	    (let ((inhibit-modification-hooks t))
	      (save-excursion
		(goto-char (+ (overlay-start twin) beg-orig))
		(insert contents)
		(delete-char (- end-orig beg-orig))
		)))))))))

(defvar mf--mirror-indicator "| ")
(add-text-properties
 0 1
 `(face (:foreground ,(format "#%02x%02x%02x" 128 128 128)
                     :background ,(format "#%02x%02x%02x" 128 128 128)))
 mf--mirror-indicator)

(provide 'multfiles)

;;; multfiles.el ends here
