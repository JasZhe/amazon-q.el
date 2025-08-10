;;; package --- Summary
;;; amazon-q-system-prompt.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: August 6, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.0"))
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
;;; Handles pseudo "system-prompt" for Q.
;;; This system prompt is also responsible for allowing us to handle font-locking code
;;; blocks more easily.
;;;
;;; Code:

(defcustom amazon-q-system-prompt-file ".amazon-q-system-prompt.md"
  "Path to system prompt file to automatically add to context."
  :type 'string
  :group 'amazon-q)

(defcustom amazon-q-code-block-begin-marker "{begin_code_block}"
 "Marker to begin code blocks."
 :type 'string
 :group 'amazon-q)

(defcustom amazon-q-code-block-end-marker "{end_code_block}"
 "Marker to end code blocks."
 :type 'string
 :group 'amazon-q)

(defcustom amazon-q-auto-generate-system-prompt t
  "Automatically generate system prompt file if it doesn't exist."
  :type 'boolean
  :group 'amazon-q)

(defconst amazon-q-default-system-prompt
  "# Amazon Q System Instructions

You are a helpful AI assistant. When providing code examples or code blocks, please follow these formatting rules:

- Begin every code block with %s
- End every code block with %s
- Place these markers on their own lines
- Keep the standard markdown code fences () inside the markers

Example format:
%s
python
def example():
    return \"Hello World\"

%s

This formatting helps with automated processing and integration with development tools.

Please follow these formatting rules consistently throughout our conversation."
 "Default content for the system prompt file.
WARNING: Be careful modifying this because fontification heavily relies on
Q sending the code block delimters.

Otherwise it makes font-locking the code blocks VERY difficult.")

(defun amazon-q--generate-default-system-prompt ()
  "Generate the default system prompt."
  (format amazon-q-default-system-prompt
          amazon-q-code-block-begin-marker
          amazon-q-code-block-end-marker
          amazon-q-code-block-begin-marker
          amazon-q-code-block-end-marker))

(defun amazon-q--ensure-system-prompt-file ()
  "Create system prompt file if it doesn't exist and auto-generation is enabled.
Per-project system prompt.
Uses `amazon-q-default-system-prompt' as content if the file is createed."
  (let ((prompt-file (expand-file-name (concat (project-root (project-current)) amazon-q-system-prompt-file))))
    (when (and amazon-q-auto-generate-system-prompt
               (not (file-exists-p prompt-file)))
      (with-temp-file prompt-file
        (insert (amazon-q--generate-default-system-prompt)))
      (message "Created Amazon Q system prompt file: %s" prompt-file))
    prompt-file))

(provide 'amazon-q-system-prompt)
;;; amazon-q-system-prompt.el ends here.
