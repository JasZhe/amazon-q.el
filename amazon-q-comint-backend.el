;;; amazon-q.el --- Comint backend for amazon q -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: August 6, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))
;; URL: https://github.com/JasZhe/amazon-q.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License:

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
;;
;;; Commentary:
;;; Multi-line prompts don't seem supported in comint (from my testing at least). In comint buffers cause C-j isn't being interpreted properly I guess.
;;; The way around it was to hook into the emacsclient buffer creation and send the region there and doing it in a way that's almost invisible to the user.
;;; Maybe there's an actual way to get the multi-line prompts to work with comint but for now this works


(defvar amazon-q--accumulated-prompt-output ""
  "Output from the previous prompt.
Used to check if amazon Q is ready to accept more commands.")

(defun amazon-q--comint-process-filter (proc string)
  "Process filter for amazon q.
Responsible for applying ansi color to the strings.
Removing some excessive whitespace.
Detecting tool permission promots."
  (when (string-match "Using tool: \\(.*\\)" string)
    (setq amazon-q--tool-requiring-permission (ansi-color-apply (match-string 1 string))))

  (when (string-match-p "Allow this action?" string)
    (if (string= (completing-read (format "Allow action %s?" amazon-q--tool-requiring-permission) '("yes" "no")) "yes")
        (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "y\r")
      (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "n\r")))

  (comint-output-filter proc string))

(defun amazon-q--comint-start (buffer)
  (let ((process-environment (append '("EDITOR=emacsclient") process-environment)))
    (with-current-buffer buffer
      (comint-mode))
    (start-process "amazon-q" buffer "bash" "-c" "q chat")
    (set-process-filter (get-buffer-process buffer) #'amazon-q--jason)
    (display-buffer buffer)))


(defun amazon-q--comint-send (buffer prompt)
  (comint-send-string (get-buffer-process buffer) (format "%s\r" prompt)))


(provide 'amazon-q-comint-backend)
