;;; amazon-q.el --- term backend for amazon q -*- lexical-binding: t; -*-
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


(defvar amazon-q--term-ready nil)

(defun amazon-q--term-process-filter (proc string)
  "Process filter for amazon q.
Responsible for applying ansi color to the strings.
Removing some excessive whitespace.
Detecting tool permission promots."
  (when (string-match-p "\x07" string)
    (message "amazon q ready!")
    (setq amazon-q--term-ready t))

  (when (string-match "Using tool: \\(.*\\)" string)
    (setq amazon-q--tool-requiring-permission (ansi-color-apply (match-string 1 string))))

  (when (string-match-p "Allow this action?" string)
    (if (string= (completing-read (format "Allow action %s?" amazon-q--tool-requiring-permission) '("yes" "no")) "yes")
        (amazon-q--send ("y"))
      (amazon-q--send "n")))
  (term-emulate-terminal proc string))

(defun amazon-q--term-start (buffer)
  (let* (;; this is so amazon q cli will send us bells if chat notifications are enabled.
         ;; we can then use the bell character as a way to check if aamzon q is ready to accept more prompts
         (term-term-name "xterm-256color")
         (process-environment (cons "EDITOR=emacsclient" process-environment)))
    (with-current-buffer buffer
      (amazon-q-term-mode)
      (term-exec buffer "amazon-q" "q" nil nil)
      (amazon-q--send "q settings chat.enableNotifications true") ;; this is useful for knowing when the process is ready to accept more input
      (set-process-filter (get-buffer-process (current-buffer)) 'amazon-q--process-filter))
    (switch-to-buffer buffer)))

(defun amazon-q--term-send (buffer prompt)
  (setq amazon-q--term-ready nil)
  (term-send-string (get-buffer-process buffer) (format "%s\r" prompt)))

(defvar amazon-q-term-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-j>") #'term-send-raw)
    map))

(define-derived-mode amazon-q-term-mode term-mode "Amazon Q Term"
  :keymap amazon-q-term-mode-map)

(defvar amazon-q--tool-requiring-permission "" "Potential tool that might require permission from the user")

(provide 'amazon-q-term-backend)
