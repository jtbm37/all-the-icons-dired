;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020  jtbm37
;; Copyright (C) 2021 Jimmy Yuen Ho Wong

;; Author: jtbm37
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 2.0
;; Keywords: files icons dired
;; Package-Requires: ((emacs "26.1") (all-the-icons "2.2.0"))
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
(require 'image)
(require 'jit-lock)
(require 'font-core)
(require 'font-lock)

(defface all-the-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon."
  :group 'all-the-icons-faces)

(defcustom all-the-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the Dired buffer."
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

(defun all-the-icons-dired--put-icon (pos)
  "Propertize POS with icon."
  (let* ((file (dired-get-filename 'relative 'noerror))
         (icon (all-the-icons-dired--icon file))
         (image (get-text-property 0 'display icon)))
    (if (or (not (eq (car image) 'image)) (member file '("." "..")))
        (put-text-property (1- pos) pos 'display
                           (if (member file '("." ".."))
                               "    "
                             (concat " " icon " ")))
      (setf (image-property image :margin) (cons (/ (window-text-width nil t) (window-text-width)) 0))
      (put-text-property (1- pos) pos 'display image))))

(defun all-the-icons-dired--fontify-region (start end &optional loudly)
  "Add icons using text properties from START to END.

START, END and the optional argument LOUDLY is passed to
`font-lock-default-fontify-region'."
  (let ((extended-region (font-lock-default-fontify-region start end loudly)))
    (when (and (consp extended-region)
               (eq (car extended-region) 'jit-lock-bounds))
      (setq start (cadr extended-region))
      (setq end (cddr extended-region)))
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (when-let ((pos (dired-move-to-filename)))
            (all-the-icons-dired--put-icon pos))
          (forward-line 1))))
    extended-region))

(defun all-the-icons-dired--setup ()
  "Set up `all-the-icons-dired'."
  (add-function :override (local 'font-lock-fontify-region-function) #'all-the-icons-dired--fontify-region)
  (setq-local font-lock-extra-managed-props (cons 'display font-lock-extra-managed-props))
  (cond (jit-lock-mode
         (jit-lock-refontify))
        (font-lock-mode
         (font-lock-fontify-region (point-min) (point-max)))))

(defun all-the-icons-dired--teardown ()
  "Tear down `all-the-icons-dired'."
  (font-lock-unfontify-buffer)
  (remove-function (local 'font-lock-fontify-region-function) #'all-the-icons-dired--fontify-region)
  (setq-local font-lock-extra-managed-props (remove 'display font-lock-extra-managed-props))
  (cond (jit-lock-mode
         (jit-lock-refontify))
        (font-lock-mode
         (font-lock-fontify-region (point-min) (point-max)))))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each file in a Dired buffer."
  :lighter " all-the-icons-dired-mode"
  (when (derived-mode-p 'dired-mode)
    (if all-the-icons-dired-mode
        (all-the-icons-dired--setup)
      (all-the-icons-dired--teardown))))

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
