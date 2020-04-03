;;; all-the-icons-dired.el --- Shows icons for each file in dired mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  jtbm37

;; Author: jtbm37
;; Version: 1.0
;; Keywords: files icons dired
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.2.0"))
;; URL: https://github.com/jtbm37/all-the-icons-dired

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

(require 'cl-lib)
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

(defvar all-the-icons-dired-mode)

(defun all-the-icons-dired--add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'all-the-icons-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun all-the-icons-dired--overlays-in (beg end)
  "Get all all-the-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'all-the-icons-dired-overlay))
   (overlays-in beg end)))

(defun all-the-icons-dired--overlays-at (pos)
  "Get all-the-icons-dired overlays at POS."
  (apply #'all-the-icons-dired--overlays-in `(,pos ,pos)))

(defun all-the-icons-dired--remove-all-overlays ()
  "Remove all `all-the-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (all-the-icons-dired--overlays-in (point-min) (point-max)))))

(defun all-the-icons-dired--refresh ()
  "Display the icons of files in a dired buffer."
  (all-the-icons-dired--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let ((icon (if (file-directory-p file)
                            (all-the-icons-icon-for-dir file
                                                        :face 'all-the-icons-dired-dir-face
                                                        :v-adjust all-the-icons-dired-v-adjust)
                          (all-the-icons-icon-for-file file :v-adjust all-the-icons-dired-v-adjust))))
              (if (member file '("." ".."))
                  (all-the-icons-dired--add-overlay (point) "  \t")
                (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
      (forward-line 1))))

(defun all-the-icons-dired--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (when all-the-icons-dired-mode
    (all-the-icons-dired--refresh)))

(defun all-the-icons-dired--setup ()
  "Setup `all-the-icons-dired'."
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (advice-add 'dired-readin :around #'all-the-icons-dired--refresh-advice)
    (advice-add 'dired-revert :around #'all-the-icons-dired--refresh-advice)
    (advice-add 'dired-internal-do-deletions :around #'all-the-icons-dired--refresh-advice)
    (advice-add 'dired-insert-subdir :around #'all-the-icons-dired--refresh-advice)
    (advice-add 'dired-do-kill-lines :around #'all-the-icons-dired--refresh-advice)
    (with-eval-after-load 'dired-narrow
      (advice-add 'dired-narrow--internal :around #'all-the-icons-dired--refresh-advice))
    (all-the-icons-dired--refresh)))

(defun all-the-icons-dired--teardown ()
  "Functions used as advice when redisplaying buffer."
  (advice-remove 'dired-readin #'all-the-icons-dired--refresh-advice)
  (advice-remove 'dired-revert #'all-the-icons-dired--refresh-advice)
  (advice-remove 'dired-internal-do-deletions #'all-the-icons-dired--refresh-advice)
  (advice-remove 'dired-narrow--internal #'all-the-icons-dired--refresh-advice)
  (advice-remove 'dired-insert-subdir #'all-the-icons-dired--refresh-advice)
  (advice-remove 'dired-do-kill-lines #'all-the-icons-dired--refresh-advice)
  (all-the-icons-dired--remove-all-overlays))

;;;###autoload
(define-minor-mode all-the-icons-dired-mode
  "Display all-the-icons icon for each files in a dired buffer."
  :lighter " all-the-icons-dired-mode"
  (when (and (derived-mode-p 'dired-mode) (display-graphic-p))
    (if all-the-icons-dired-mode
        (all-the-icons-dired--setup)
      (all-the-icons-dired--teardown))))

(provide 'all-the-icons-dired)
;;; all-the-icons-dired.el ends here
