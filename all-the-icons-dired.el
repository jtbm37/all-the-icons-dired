;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  matrix

;; Author: jtbm37
;; Keywords: icons, dired
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.2.0"))

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
;;

;;; Code:
(require 'dired)
(require 'all-the-icons)

(defvar all-the-icons-dired-dir-face nil
  "Face for the directory icon")

;; (defvar all-the-icons-dired-file-face nil
;;   "Face for the file icon")

(defun dired-icon--get-files ()
  "List all files in the current dired buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
	(let ((file (dired-get-filename 'verbatim t)))
	  (unless (member file '("." ".."))
	    (let ((filename (dired-get-filename nil t)))
	      (if (file-directory-p filename)
		  (insert (concat (all-the-icons-octicon "file-directory" :v-adjust 0.01 :face all-the-icons-dired-dir-face) " "))
		(insert (concat (all-the-icons-icon-for-file file :v-adjust 0.01 ;; :face all-the-icons-dired-file-face
							     ) " ")))))))
      (forward-line 1))))

(defun all-the-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (let ((inhibit-read-only t))
    (dired-icon--get-files)))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each files in a dired buffer."
  (if t
      (progn
	(add-hook 'dired-after-readin-hook 'all-the-icons-dired--display)
	(when (eq major-mode 'dired-mode)
	  (all-the-icons-dired--display))
	(message "all-the-icons-dired-mode activated"))
    (remove-hook 'dired-after-readin-hook 'all-the-icons-dired--display)))

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
