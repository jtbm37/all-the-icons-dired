;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  jtbm37

;; Author: jtbm37
;; Version: 1.0
;; Keywords: files icons dired
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
;; To use this package, simply add this to your init.el:
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; To manually install, add this to your init.el before the hook mentioned above.
;; (add-to-load-path (expand-file-name "~/path/to/all-the-icons-dired"))
;; (load "all-the-icons-dired.el")


;;; Code:
(require 'dired)
(require 'all-the-icons)

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'all-the-icons
  :type 'number)

(defvar-local all-the-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defcustom all-the-icons-dired-icon-args nil
  "Initial list of arguments passed to all-the-icons icon functions."
  :group 'all-the-icons
  :type 'sexp)

(defun all-the-icons-dired--icon-for-filename (file filename &optional remote-p)
  (let ((icon-args `(,@all-the-icons-dired-icon-args :v-adjust ,all-the-icons-dired-v-adjust))
        icon-func icon-name)
    (if (file-directory-p filename)
        (progn
          (setq icon-args
                (append `(:face all-the-icons-dired-dir-face) icon-args))

          (cond
           (remote-p
            (setq icon-func 'all-the-icons-octicon
                  icon-name "file-directory"))
           ((file-symlink-p filename)
            (setq icon-func 'all-the-icons-octicon
                  icon-name "file-symlink-directory"))
           ((all-the-icons-dir-is-submodule filename)
            (setq icon-func 'all-the-icons-octicon
                  icon-name "file-submodule"))
           ((file-exists-p (format "%s/.git" filename))
            (setq icon-func 'all-the-icons-octicon
                  icon-name "repo"))
           (t
            (let ((matcher
                    (all-the-icons-match-to-alist file all-the-icons-dir-icon-alist)))
              (setq icon-func (car matcher)
                    icon-name (cadr matcher))))))
      (setq icon-func 'all-the-icons-icon-for-file
            icon-name file))

    (apply icon-func icon-name icon-args)))

(defun all-the-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not all-the-icons-dired-displayed) dired-subdir-alist)
    (setq-local all-the-icons-dired-displayed t)
    (let ((inhibit-read-only t)
	        (remote-p (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p default-directory))))
      (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
	        (when (dired-move-to-filename nil)
	          (let ((file (dired-get-filename 'verbatim t)))
	            (unless (member file '("." ".."))
		            (let ((filename (dired-get-filename nil t)))
                  (insert (all-the-icons-dired--icon-for-filename file filename remote-p) " ")
		              ))))
	        (forward-line 1))))))

(defun all-the-icons-dired--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local all-the-icons-dired-displayed nil))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " all-the-icons-dired-mode"
  (if (and (display-graphic-p) all-the-icons-dired-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'all-the-icons-dired--display t t)
        (when (derived-mode-p 'dired-mode)
          (all-the-icons-dired--display)))
    (remove-hook 'dired-after-readin-hook 'all-the-icons-dired--display t)
    (dired-revert)))

(advice-add 'dired-revert :before #'all-the-icons-dired--reset)

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
