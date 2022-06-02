;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020  jtbm37
;; Copyright (C) 2021 Jimmy Yuen Ho Wong

;; Author: jtbm37
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 2.0
;; Keywords: files icons dired
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.2.0"))
;; URL: https://github.com/wyuenho/all-the-icons-dired

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
(require 'subr-x)

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'all-the-icons
  :type 'number)

(defcustom all-the-icons-dired-monochrome t
  "Whether to show the icons as the same color as the text on the same line."
  :group 'all-the-icons
  :type 'boolean)

(defvar all-the-icons-dired-mode)

(defun all-the-icons-dired--icon (file)
  "Return the icon for FILE."
  (if (file-directory-p file)
      (all-the-icons-icon-for-dir file
                                  :face 'all-the-icons-dired-dir-face
                                  :v-adjust all-the-icons-dired-v-adjust)
    (apply 'all-the-icons-icon-for-file file
           (append
            `(:v-adjust ,all-the-icons-dired-v-adjust)
            (when all-the-icons-dired-monochrome
              `(:face ,(face-at-point)))))))

(defun all-the-icons-dired--put-icon (pos) "Propertize POS with icon."
       (let* ((file (dired-get-filename 'relative 'noerror))
              (icon (all-the-icons-dired--icon file)))
         (put-text-property (1- pos) pos 'display
                            (if (member file '("." ".."))
                                "    "
                              (concat " " icon " ")))))

(defun all-the-icons-dired--propertize (&optional beg end &rest _)
  "Add icons using text properties from BEG to END.
They defualt to `(point-min)' and `(point-max)'."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (with-silent-modifications
      (ignore-errors
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (when-let ((pos (dired-move-to-filename)))
              (all-the-icons-dired--put-icon pos))
            (forward-line 1)))))))

(defun all-the-icons-dired--setup ()
  "Setup `all-the-icons-dired'."
  (add-hook 'dired-after-readin-hook #'all-the-icons-dired--propertize)
  (advice-add 'dired-insert-set-properties :before #'all-the-icons-dired--propertize))

(defun all-the-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (remove-hook 'dired-after-readin-hook #'all-the-icons-dired--propertize)
  (advice-remove 'dired-insert-set-properties #'all-the-icons-dired--propertize))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each file in a dired buffer."
  :lighter " all-the-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if all-the-icons-dired-mode
        (all-the-icons-dired--setup)
      (all-the-icons-dired--teardown))))

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
