;;; helm-switch-frame.el --- Switch frames with the help of helm -*- lexical-binding: t -*-

;; Copyright © 2015 Tarmo Aidantausta

;; Author: Tarmo Aidantausta <tarmo@hiutale.org>
;; URL: https://github.com/bleadof/helm-switch-frame
;; Keywords: helm, frame, navigation
;; Version: 1.0.0
;; Package-Requires: ((helm "1.7.9") (dash "2.12.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Switch frame with the help of helm
;;
;;; Code:

(require 'helm)
(require 'dash)

(defun hsf/switch-to-frame (frame-name)
  (select-frame-set-input-focus (cdr (assoc frame-name (frame-names-with-frame)))))

(defun hsf/projectile-root-for-directory (directory)
  (--some (let* ((cache-key (format "%s-%s" it directory))
                 (cache-value (gethash cache-key projectile-project-root-cache)))
            (if cache-value
                cache-value
              (funcall it (file-truename directory))))
          projectile-project-root-files-functions))

(defun hsf/projectile-project-name-for-project-root (project-root)
  (file-name-nondirectory (directory-file-name project-root)))

(defun hsf/projectile-project-names-for-frame-buffers (frame)
  (-map
   (lambda (buffer)
     (-when-let (filename
                 (buffer-file-name buffer))
       (hsf/projectile-project-name-for-project-root (hsf/projectile-root-for-directory (file-name-directory filename)))))
   (frame-parameter frame 'buffer-list)))

(defun hsf/projectile-project-name-for-frame (frame)
  (car
   (-max-by (-on '> 'length)
            (-group-by
             'identity
             (-filter
              'identity
              (hsf/projectile-project-names-for-frame-buffers frame))))))

(defun hsf/first-buffer-name (frame)
  (-if-let (first-buffer (car (frame-parameter frame 'buffer-list)))
      (-if-let (filename (buffer-file-name first-buffer))
          (file-name-nondirectory filename)
        (buffer-name first-buffer))
    ""))

(defun hsf/maybe-with-project-name (frame)
  (let ((buffer-name (hsf/first-buffer-name frame)))
    (if (boundp 'projectile-mode)
        (-if-let (project-name (hsf/projectile-project-name-for-frame frame))
            (format "[%s] %s" project-name buffer-name)
          buffer-name)
      buffer-name)))

(defun hsf/frame-candidate-name (frame)
  (format "[%s] %s" (frame-parameter frame 'window-id) (hsf/maybe-with-project-name frame)))

(cl-defun hsf/frame-names-with-frame (&optional (with 'identity))
  "List of frame names and associated frame with optional filter."
  (-map (lambda (frame)
          (cons (hsf/frame-candidate-name frame) frame))
        (-filter with (frame-list))))

(defun hsf/frame-names-besides-current ()
  "List of frame names, but not the current frame"
  (-map 'car (hsf/frame-names-with-frame
              (lambda (frame)
                (not (equal frame (selected-frame)))))))

(setq helm-frame-source
      `((name . "Switch to frame")
        (candidates . hsf/frame-names-besides-current)
        (action . (lambda (candidate)
                    (hsf/switch-to-frame candidate)))))

(defun helm-switch-frame ()
  (interactive)
  (helm :sources '(helm-frame-source)))

(provide 'helm-switch-frame)

;;; helm-switch-frame.el ends here
