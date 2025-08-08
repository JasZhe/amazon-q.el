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


(defvar amazon-q--comint-accumulated-prompt-output ""
  "Output from the previous prompt.")

(defun amazon-q--comint-process-filter (proc string)
  "Process filter for amazon q."

  (let ((clean-string (ansi-color-filter-apply string)))
    (setq amazon-q--comint-accumulated-prompt-output (concat amazon-q--comint-accumulated-prompt-output clean-string))
    ;;  is a carriage return represented by /r
    ;; also check for any of the permutations of that loading spinner character
    (setq amazon-q--comint-accumulated-prompt-output
          (replace-regexp-in-string "\r[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]\\s-*Thinking\\.\\.\\." "" amazon-q--comint-accumulated-prompt-output))

    (when (string-match "Using tool: \\(.*\\)" amazon-q--comint-accumulated-prompt-output)
      (setq amazon-q--tool-requiring-permission (match-string 1 amazon-q--comint-accumulated-prompt-output)))

    (when (string-match-p "Allow this action?" string)
      (if (string= (completing-read (format "Allow action %s?" amazon-q--tool-requiring-permission) '("yes" "no")) "yes")
          (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "y\r")
        (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "n\r"))))
  (comint-output-filter proc string))


(defun amazon-q--comint-start (buffer)
  (let ((process-environment (append '("EDITOR=emacsclient") process-environment)))
    (with-current-buffer buffer
      (amazon-q-comint-mode))
    (start-process "amazon-q" buffer "bash" "-c" "q chat")
    (set-process-filter (get-buffer-process buffer) #'amazon-q--comint-process-filter)
    (display-buffer buffer)))


(defun amazon-q--comint-send (buffer prompt)
  "Calls comint-send-string but clears amazon-q--comint-accumulated-prompt-output before hand."
  (setq amazon-q--comint-accumulated-prompt-output "") ;; new prompt need to reset to ""
  (comint-send-string (get-buffer-process buffer) (format "%s\r" prompt)))


(define-derived-mode amazon-q-comint-mode comint-mode "Amazon Q"
  "Major mode for Amazon Q chat sessions with the comint backend."
  ;; need to unset the accumulated prompt output in the case where the user is interacting with amazon q
  ;; outside of amazon-q--comint-send
  (add-hook 'comint-input-filter-functions (lambda (_) (setq amazon-q--comint-accumulated-prompt-output "")) nil t))


(provide 'amazon-q-comint-backend)
