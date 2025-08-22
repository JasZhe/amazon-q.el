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

(defvar amazon-q--term-accumulated-prompt-output ""
  "Output from the previous prompt.")

(defvar-local amazon-q--term-ready nil)
(defvar-local amazon-q--term-timer-fn nil)

(defvar amazon-q--term-tool-requiring-permission "" "Potential tool that might require permission from the user")

(defvar amazon-q--term-callback nil
  "Callback fn when the Amazon Q process is ready for more input.

Should take no input.
Will be called when Q sends a BELL (\x07) signalling that the previous command finished.")

(defun amazon-q--term-process-filter (proc string)
  "Process filter for amazon q.
Responsible for applying ansi color to the strings.
Removing some excessive whitespace.
Detecting tool permission promots."
  (when (string-match-p "\x07" string)
    (message "amazon q ready!")
    (setq amazon-q--term-ready t))

  (let ((clean-string (ansi-color-filter-apply string)))
    (setq amazon-q--term-accumulated-prompt-output (concat amazon-q--term-accumulated-prompt-output clean-string))
    ;;  is a carriage return represented by /r
    ;; also check for any of the permutations of that loading spinner character
    (setq amazon-q--term-accumulated-prompt-output
          (replace-regexp-in-string "\r[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]\\s-*Thinking\\.\\.\\." "" amazon-q--term-accumulated-prompt-output))
    (setq amazon-q--term-accumulated-prompt-output
          (replace-regexp-in-string "\r" "" amazon-q--term-accumulated-prompt-output))

    (when (string-match "Using tool: \\(.*\\)" string)
      (setq amazon-q--term-tool-requiring-permission (ansi-color-apply (match-string 1 string))))

    (when (string-match-p "Allow this action?" string)
      (if (string= (completing-read (format "Allow action %s?" amazon-q--term-tool-requiring-permission) '("yes" "no")) "yes")
          (term-send-string proc "y")
        (term-send-string proc "n")))

    (prog1
        (term-emulate-terminal proc string)
      (when (and amazon-q--term-ready amazon-q--term-callback)
        (condition-case nil
            (unwind-protect
                (progn
                  (funcall amazon-q--term-callback))
              (setq amazon-q--term-callback nil))
          (quit nil)
          (t (message "Amazon Q error in callback.")))))))

(defun amazon-q--term-ready-context-files (buffer)
  (let ((files '()))
    (with-current-buffer buffer
      (end-of-buffer)
      (re-search-backward "matched files in use:")
      (forward-line 1)
      (while (not (looking-at "^\\s-*$"))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (string-match "^[^/]*\\(.*\\) (.*tkns)$" line)
          (push (match-string 1 line) files))
        (forward-line 1)))
    files))

(defun amazon-q--term-start (buffer)
  (let* (;; this is so amazon q cli will send us bells if chat notifications are enabled.
         ;; we can then use the bell character as a way to check if aamzon q is ready to accept more prompts
         (term-term-name "xterm-256color")
         (process-environment (cons "EDITOR=emacsclient" process-environment)))
    (with-current-buffer buffer (amazon-q-term-mode))

    ;; if term buffer isn't ready to accept input, then sending commands gets kinda wonky at least for the first command
    ;; it just inputs the newlines but doesn't actually "send" it to amazon q or something..
    (while (not amazon-q--term-ready)
      (sleep-for 0.1))
    (switch-to-buffer buffer)))

(defun amazon-q--term-send (buffer prompt)
  (with-current-buffer buffer
    (while (not amazon-q--term-ready)
      (sleep-for 0.1))
    (setq amazon-q--term-ready nil)
    (term-send-raw-string (format "%s" prompt)))
  (setq amazon-q--term-accumulated-prompt-output ""))

(defun amazon-q--term-check-ready (buffer)
  (with-current-buffer buffer
    (unless amazon-q--term-ready
      (setq amazon-q--term-ready
            (string-match-p "^\\(\\[.*\\] > \\|> \\)"
                            (save-excursion
                              (goto-char (point-max))
                              (while (and (not (bobp))
                                          (looking-at "^\\s-*$"))
                                (forward-line -1))
                              (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
    amazon-q--term-ready))

(defun amazon-q--term-cleanup ()
  "Cleans up leftover elisp objects from the amazon q session.
For instance, timers."
  (message "Cleaning up amazon q term buffer")
  (cancel-timer amazon-q--term-timer-fn))

(define-derived-mode amazon-q-term-mode term-mode "Amazon Q Term"
  :keymap amazon-q-term-mode-map
  (setq-local term-prompt-regexp "^\\(\\[.*\\]\\)? > ")
  (setq amazon-q--term-ready nil)
  (term-exec buffer "amazon-q" "q" nil nil)
  ;; for now we spawn a timer for each amazon q process
  ;; there might be a better way to manage it but this is the simplest
  ;; we just have to make sure to clean up after ourselves
  (setq amazon-q--term-timer-fn (run-with-timer 0.3 0.3 #'amazon-q--term-check-ready buffer))
  (add-hook 'kill-buffer-hook #'amazon-q--term-cleanup nil t)
  (set-process-filter (get-buffer-process (current-buffer)) 'amazon-q--term-process-filter)
  )

(provide 'amazon-q-term-backend)
