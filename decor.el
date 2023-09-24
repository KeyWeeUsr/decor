;;; decor.el --- Modify visual decorations

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: window, decoration, distraction, x11, xprop
;; Version: 1.0.0

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

;; This library attempts to simplify removal of all frame (window) decorations
;; via two simple functions and possibly some constants later for
;; customization.

;;; Code:

(defun decor-check-bin (buff-name cmd)
  (inline)
  (when (eq (executable-find cmd) nil)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create buff-name))
      (insert (format "'%s' not found\n" cmd)))
    t))

(defun decor-check-deps ()
  (inline)
  (let ((buff-name "*decor deps*")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; binaries
    (dolist (item (list "xprop"))
      (when (decor-check-bin buff-name item) (setq failed t)))

    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (user-error "Some deps are missing"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun decor-toggle-single-frame (win-id on)
  (call-process "xprop"
                nil nil nil
                "-id" win-id "-format" "_MOTIF_WM_HINTS" "32c"
                "-set" "_MOTIF_WM_HINTS" (if (eq on t) "1" "2")))

(defun decor-toggle-all-frames (on)
  (dolist (frame (frame-list))
    (decor-toggle-single-frame (frame-parameter frame 'outer-window-id) on)))

(defun decor-all-frames-on ()
  (interactive)
  (decor-check-deps)
  (decor-toggle-all-frames t))

(defun decor-all-frames-off ()
  (interactive)
  (decor-check-deps)
  (decor-toggle-all-frames nil))

(provide 'decor)
;;; decor.el ends here
