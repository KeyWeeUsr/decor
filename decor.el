;;; decor.el --- Modify visual decorations -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, window, decoration, distraction, xprop, xwayland
;; Version: 1.1.0
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/KeyWeeUsr/decor

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

(defgroup decor
  nil
  "Customization group for `decor-mode'."
  :group 'convenience
  :group 'external
  :group 'frames
  :group 'x)

(defcustom decor-always-check-deps
  t
  "Always look up binaries, libraries and other required tools."
  :group 'decor
  :type 'boolean)

(defun decor--check-bin (buff-name cmd)
  "Check if a binary is present on the system.
Argument BUFF-NAME destination to write failure to.
Argument CMD name of the checked binary."
  (when (eq (executable-find cmd) nil)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create buff-name))
      (insert (format "'%s' not found\n" cmd)))
    t))

(defun decor--check-deps ()
  "Check if all deps are present on the system."
  (let ((buff-name "*decor deps*")
        (failed nil))
    ;; clean first
    (kill-buffer (get-buffer-create buff-name))

    ;; binaries
    (when (or (eq window-system "x")
              (and (eq (getenv "XDG_SESSION_TYPE") "wayland")
                   (decor--check-bin buff-name "Xwayland")))
      (dolist (item (list "xprop"))
        (when (decor--check-bin buff-name item) (setq failed t))))

    (if (eq failed t)
        (progn
          (switch-to-buffer (get-buffer-create buff-name))
          (error "Some deps are missing"))
      (kill-buffer (get-buffer-create buff-name)))))

(defun decor-toggle-single-frame (win-id on)
  "Toggle decorations of a single frame.
Argument WIN-ID frame's window ID.
Argument ON t/nil to enable/disable."
  (call-process "xprop"
                nil nil nil
                "-id" win-id "-format" "_MOTIF_WM_HINTS" "32c"
                "-set" "_MOTIF_WM_HINTS" (if (eq on t) "1" "2")))

(defun decor-toggle-all-frames (on)
  "Toggle decorations ON (t) or off (nil) for all Emacs frames."
  (dolist (frame (frame-list))
    (decor-toggle-single-frame (frame-parameter frame 'outer-window-id) on)))

(defun decor-all-frames-on ()
  "Toggle decorations on for all Emacs frames."
  (interactive)
  (when decor-always-check-deps
    (decor--check-deps))
  (decor-toggle-all-frames t))

(defun decor-all-frames-off ()
  "Toggle decorations off for all Emacs frames."
  (interactive)
  (when decor-always-check-deps
    (decor--check-deps))
  (decor-toggle-all-frames nil))

;;;###autoload
(define-minor-mode decor-mode
  "Trigger custom decoration of all Emacs desktop Frames."
  :global t
  :group 'decor
  (if decor-mode
      (decor-all-frames-off)
    (decor-all-frames-on)))

(provide 'decor)
;;; decor.el ends here
