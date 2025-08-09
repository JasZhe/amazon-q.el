;;; amazon-q.el -*- lexical-binding: t; -*-
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

(require 'transient)
(require 'amazon-q-term-backend)
(require 'amazon-q-comint-backend)

(defgroup amazon-q nil
  "Amazon Q interface for Emacs."
  :group 'tools)

(defcustom amazon-q-backend 'comint
  "Backend to use for Amazon Q."
  :type '(choice
          (const :tag "Term backend" term)
          (const :tag "Comint backend" comint))
  :group 'amaozn-q)

(defcustom amazon-q-system-prompt-file "~/.amazon-q-system-prompt.md"
  "Path to system prompt file to automatically add to context."
  :type 'string
  :group 'amazon-q)

(defcustom amazon-q-auto-generate-system-prompt t
  "Automatically generate system prompt file if it doesn't exist."
  :type 'boolean
  :group 'amazon-q)

(defconst amazon-q-default-system-prompt
  "# Amazon Q System Instructions

You are a helpful AI assistant. When providing code examples or code blocks, please follow these formatting rules:

- Begin every code block with {begin_code_block}
- End every code block with {end_code_block}
- Place these markers on their own lines
- Keep the standard markdown code fences () inside the markers

Example format:
{begin_code_block}
python
def example():
    return \"Hello World\"

{end_code_block}

This formatting helps with automated processing and integration with development tools.

Please follow these formatting rules consistently throughout our conversation."
 "Default content for the system prompt file.")

(defcustom amazon-q-code-block-begin-marker "{begin_code_block}"
 "Marker to begin code blocks."
 :type 'string
 :group 'amazon-q)

(defcustom amazon-q-code-block-end-marker "{end_code_block}"
 "Marker to end code blocks."
 :type 'string
 :group 'amazon-q)

(defun amazon-q--ensure-system-prompt-file ()
  "Create system prompt file if it doesn't exist and auto-generation is enabled.
Uses `amazon-q-default-system-prompt' as content if the file is createed."
  (let ((prompt-file (expand-file-name amazon-q-system-prompt-file)))
    (when (and amazon-q-auto-generate-system-prompt
               (not (file-exists-p prompt-file)))
      (with-temp-file prompt-file
        (insert amazon-q-default-system-prompt))
      (message "Created Amazon Q system prompt file: %s" prompt-file))
    prompt-file))

(defvar amazon-q-region-context-filename ".amazonq-context"
  "Per-project file to contain context we can specify by region,
rather than adding an entire file's worth of context.")

(defun amazon-q--ensure-context-file ()
  "Create temporary per-project context file if it doesn't exist."
  (let ((context-file (expand-file-name (concat (project-root (project-current)) amazon-q-region-context-filename))))
    (when (not (file-exists-p context-file))
      (with-temp-file context-file))
    context-file))

(defvar amazon-q--code-block-regex
  (format "%s\n\\(?:\\([a-zA-Z0-9+-]+\\)\\)?\n\\(\\(?:.\\|\n\\)*?\\)
\n%s"
          (regexp-quote amazon-q-code-block-begin-marker)
          (regexp-quote amazon-q-code-block-end-marker))
  "Regex to match code blocks with custom markers.")

(defun amazon-q--get-buffer-create ()
  (when (project-current)
    (get-buffer-create (concat "*amazon-q [" (project-name (project-current)) "]"))))


(defun amazon-q-start ()
  (interactive)
  (let ((system-prompt-file (amazon-q--ensure-system-prompt-file))
        (context-file (amazon-q--ensure-context-file)))
    (if (get-buffer-process (amazon-q--get-buffer-create))
        (switch-to-buffer (amazon-q--get-buffer-create))
      (cond ((eq amazon-q-backend 'term) (amazon-q--term-start (amazon-q--get-buffer-create)))
            ((eq amazon-q-backend 'comint) (amazon-q--comint-start (amazon-q--get-buffer-create))))
      (amazon-q--send (format "/context add %s" context-file))
      (amazon-q--send (format "/context add %s" system-prompt-file)))))


(defun amazon-q--send (prompt)
  (cond ((eq amazon-q-backend 'term) (amazon-q--term-send (amazon-q--get-buffer-create) prompt))
        ((eq amazon-q-backend 'comint) (amazon-q--comint-send (amazon-q--get-buffer-create) prompt))))


(defvar amazon-q-last-client-buffer nil
  "Last buffer opened via emacsclient.")

(defun amazon-q-track-client-buffer ()
  "Track buffers opened via emacsclient."
  (setq amazon-q-last-client-buffer (current-buffer)))

(defvar amazon-q--text-to-send nil "Text to be sent in the emacsclient buffer for amazon q.")
(defvar amazon-q--send-region-immediately nil
  "If t, the /editor buffer will immediately be sent via `server-edit'.
Otherwise the user will have the opportunity to edit the prompt before being submitted to amazon q.
see: `amazon-q--send-region'")

(defun amazon-q--editor-buffer-insert-region ()
  "Intended to be used with `server-switch-hook'.
When emascclient is invoked, this will insert the saved region from
`amazon-q--send-region' to the emacsclient buffer.
If `amazon-q--send-region-immediately' is set, `server-edit' will be called
immediately.
Otherwise, the user will have the opportunity to edit the emacsclient buffer."
  (when amazon-q--text-to-send
    (insert amazon-q--text-to-send)
    (save-buffer)
    (setq amazon-q--text-to-send nil)
    (if amazon-q--send-region-immediately
        (progn
          (message "Sending region to amazon Q immediately..")
          (server-edit))
      (message "Region inserted. Edit the prompt and submit when ready."))
    (display-buffer amazon-q--buffer)))

(add-hook 'server-visit-hook 'amazon-q-track-client-buffer)
(add-hook 'server-switch-hook 'amazon-q--editor-buffer-insert-region)

(defun amazon-q-send-region (arg)
  "Send the current region to amazon Q.
With a prefix ARG, edit the prompt before sending."
  (interactive "P")
  (setq amazon-q--send-region-immediately (not arg))
  (setq amazon-q--buffer (amazon-q--get-buffer-create))
  (setq amazon-q--text-to-send (buffer-substring-no-properties (region-beginning) (region-end)))
  (amazon-q--send "/editor"))


(defun amazon-q-send-region-as-context (arg)
  "Send the current region to amazon Q.
With a prefix ARG, edit the prompt before sending."
  (interactive "P")
  (let ((context-file (expand-file-name (concat (project-root (project-current)) amazon-q-region-context-filename))))
    (with-temp-file context-file
      (insert (buffer-substring-no-properties (region-beginning) (region-end)))
      (insert "\n"))))

(defun amazon-q-send-defun (arg)
  "Send the defun at point to amazon Q.
With a prefix ARG, edit the prompt before sending."
  (interactive "P")
  (setq amazon-q--send-region-immediately (not arg))
  (setq amazon-q--buffer (amazon-q--get-buffer-create))
  (setq amazon-q--text-to-send (thing-at-point 'defun))
  (amazon-q--send "/editor"))


(defun amazon-q-explain-error (arg)
  "Send the flymake diagnostic at point to amazon Q.
With a prefix ARG, edit the prompt before sending."
  (interactive "P")
  (setq amazon-q--send-region-immediately (not arg))
  (setq amazon-q--buffer (amazon-q--get-buffer-create))
  (if-let ((diagnostics (flymake-diagnostics (1- (point)) (1+ (point)))))
      (progn
        (setq amazon-q--text-to-send (flymake-diagnostic-text (first diagnostics)))
        (amazon-q--send "/editor"))
    (message "No diagnostic at point.")))

(defun amazon-q-clear-context ()
  "Run /clear to clear amazon Q context."
  (interactive)
  (amazon-q--send "/clear")
  (amazon-q--send "y"))

(defun amazon-q-compact-context ()
  "Run /compact to compact amazon Q context."
  (interactive)
  (amazon-q--send "/compact"))

(defun amazon-q-add-file-to-context ()
  "Prompt for a file to add to amazon Q context."
  (interactive)
  (let ((file-to-add (read-file-name "Add to context: ")))
    (amazon-q--send (format "/context add %s" file-to-add))))

(defun amazon-q-remove-file-from-context ()
  (interactive)
  (amazon-q--send "/context show")

  (cond ((eq amazon-q-backend 'term) (message "TODO"))
        ((eq amazon-q-backend 'comint)
         (setq amazon-q--comint-callback #'amazon-q--remove-file-from-context-callback))))

(defun amazon-q--remove-file-from-context-callback ()
  "Prompt for a file to removed from th amazon Q context."
  (interactive)
  (let ((files '()))
    (dolist (line (split-string amazon-q--comint-accumulated-prompt-output "\n"))
      (cond
           ;; Handle files with match counts: "~/path/file.ext (1 match)"
           ((string-match "^[[:space:]]*\\([^[:space:]]+\\.[a-zA-Z]+\\)[[:space:]]*(\\([0-9]+\\) match)" line)
            (push (match-string 1 line) files))
           ;; Handle wildcard patterns: ".amazonq/rules/**/*.md"
           ((string-match "^[[:space:]]*\\([^[:space:]]+/\\*\\*/\\*\\.[a-zA-Z]+\\)[[:space:]]*$" line)
            (push (match-string 1 line) files))
           ;; Handle simple filenames: "AmazonQ.md", "README.md"
           ((string-match "^[[:space:]]*\\([^[:space:]]+\\.[a-zA-Z]+\\)[[:space:]]*$" line)
            (push (match-string 1 line) files))))
    (setq files (nreverse files))
    (let ((file-to-remove (completing-read "Remove file from context: " files)))
      (if file-to-remove
          (amazon-q--send (format "/context remove %s" file-to-remove))
        (message "No file selected.")))))

(transient-define-prefix amazon-q-transient ()
  "Amazon Q Menu."
  ["Amazon Q"
   ["Session Management"
    ("q" "Start or switch to Amazon Q buffer" amazon-q-start)
    ]
   ["Quick actions"
    ("r" "Send region." amazon-q-send-region)
    ("R" "Add region as context." amazon-q-send-region-as-context)
    ("d" "Send defun at point." amazon-q-send-defun)
    ("e" "Explain diagnostic/error at point." amazon-q-explain-error)
    ]
   ["Slash commands."
    ("f" "Add a file to context." amazon-q-add-file-to-context)
    ("c" "Compact context." amazon-q-compact-context)
    ("x" "Clear context." amazon-q-clear-context)
    ]
   ])



(provide 'amazon-q)
