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
;;;
;;; Code:
(require 'amazon-q-system-prompt)

(defvar amazon-q--comint-accumulated-prompt-output ""
  "Output from the previous prompt.")

(defvar amazon-q--comint-ready-for-input nil
  "Whether Amazon Q is ready to accept new input.")

(defvar amazon-q--comint-callback nil
  "Callback fn when the Amazon Q process is ready for more input.

Should take no input.
To get the Amazon Q prompt response see: `amazon-q--accumulated-prompt-output'.

Warning: \"readiness\" is heuristics-based so may not work
perfectly all the time.

We simply check to see if a new cli prompt got outputted by the process
i.e. \"^ >$\"")


(defvar amazon-q--comint-tool-requiring-permission ""
  "Potential tool that might require permission from the user.")

(defun amazon-q--comint-process-filter (proc string)
  "Process filter for amazon q.

Accumuates STRING from PROC into `amazon-q--comint-accumulated-prompt-output'
that we can then use to check if Q is ready to accept more input.

Checks to see if any yes/no prompts require user interaction.

Lastly, if we determine Q is done spewing output, calls
`amazon-q--comint-callback' if specified."

  (let ((clean-string (ansi-color-filter-apply string)))
    (setq amazon-q--comint-accumulated-prompt-output (concat amazon-q--comint-accumulated-prompt-output clean-string))
    ;;  is a carriage return represented by /r
    ;; also check for any of the permutations of that loading spinner character
    (setq amazon-q--comint-accumulated-prompt-output
          (replace-regexp-in-string "\r[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]\\s-*Thinking\\.\\.\\." "" amazon-q--comint-accumulated-prompt-output))

    (setq amazon-q--comint-ready-for-input (string-match-p "> $" string))

    (when (string-match "Using tool: \\(.*\\)" amazon-q--comint-accumulated-prompt-output)
      (setq amazon-q--comint-tool-requiring-permission (match-string 1 amazon-q--comint-accumulated-prompt-output)))

    (when (string-match-p "Allow this action?" string)
      (if (string= (completing-read (format "Allow action %s?" amazon-q--comint-tool-requiring-permission) '("yes" "no")) "yes")
          (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "y\r")
        (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "n\r"))))
  (prog1
      (comint-output-filter proc string)
    (when (and amazon-q--comint-ready-for-input amazon-q--comint-callback)
      ;; don't want 'error in process filter' cause of the callback
      (condition-case nil
          (unwind-protect
              (funcall amazon-q--comint-callback)
            (setq amazon-q--comint-callback nil))
         (t (message "Amazon Q error in callback."))))))


(defun amazon-q--comint-start (buffer)
  "Start a Amazon Q in BUFFER.
Amazon Q needs some sort of shell to be able to interact with it.
See docs for supported command line environments.
Bash, Zsh, and Fish are explicitly listed.

sh probably works too, but it's not specified in the docs so we'll stick
with bash since most users will have bash at least."
  (let ((process-environment (append '("EDITOR=emacsclient") process-environment)))
    (with-current-buffer buffer
      (amazon-q-comint-mode))
    (start-process "amazon-q" buffer "bash" "-c" "q chat")
    (set-process-filter (get-buffer-process buffer) #'amazon-q--comint-process-filter)
    (display-buffer buffer)))


(defun amazon-q--comint-send (buffer prompt)
  "Call `comint-send-string' with PROMPT for BUFFER.
Clears `amazon-q--comint-accumulated-prompt-output' before hand."
  (setq amazon-q--comint-accumulated-prompt-output "") ;; new prompt need to reset to ""
  (setq amazon-q--comint-ready-for-input nil)
  (comint-send-string (get-buffer-process buffer) (format "%s\r" prompt)))


(defun amazon-q--comint-wait-for-process-to-be-ready (buffer)
  "Spin until amazon Q BUFFER process is ready to accept more input.
Will most likely stay unused because we don't have a guaranteed way to
detect \"readiness\".

Using `amazon-q--comint-callback' is preferred."
  (let ((timeout 1)
        (start-time (current-time)))
    (while (and (not amazon-q--comint-ready-for-input)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sleep-for 0.1)
      (accept-process-output (get-buffer-process buffer) 0.1))))

(defun amazon-q--comint-fontify-src-blocks ()
  "Fontifies body blocks for detected languages.
Essentially creates a temporary buffer with the specified major
mode and inserts the request body text into it.

Then we can copy the font properties from that
temporary buffer to our Amazon Q comint buffer.

This is basically a simplified version of the combination of
`org-fontify-meta-lines-and-blocks-1' and `org-src-font-lock-fontify-block'
since we don't need to care about the other block types in org.

This is NOT to be used in `font-lock-defaults' for performance reasons.
This should be run after the latest prompt was processed to fontify
the source blocks from the most recent amazon q output.

This overrides existing font-locking via font-lock mode
in the buffer via modifying `font-lock-face'."
  (save-excursion
    (comint-previous-prompt 1)
    (while (re-search-forward
            (rx-to-string
             `(: bol
                ;; weird spacing issues if the FIRST thing it spits out is a code block
                (? any " Thinking...> ")
                (group ,amazon-q-code-block-begin-marker "\n" (group (zero-or-more (any "a-zA-Z"))))))
            nil t)
      ;; some text is read-only which prohibits us from adding text props
      ;; so we temporarily inhibit it only in this function
      (let ((inhibit-read-only t)
            (beg (match-beginning 0))
            (block-start (match-end 0))
            (block-end nil)
            (lang (match-string 2))
            end)
        (when (re-search-forward (rx-to-string `(group bol ,amazon-q-code-block-end-marker)))
          (setq block-end (match-beginning 0))
          (setq end (match-end 0))

          ;; beg and end include the block markers
          ;; block-start and block-end only include the code block itself
          (remove-text-properties beg end '(face nil))
          (remove-text-properties beg end '(read-only nil))
          (add-text-properties beg end '(font-lock-fontified t font-lock-multiline t))

          ;; this save match data is really important or else we only fontify a single block
          (save-match-data
            (remove-overlays block-start block-end)
            (let ((string (buffer-substring-no-properties block-start block-end))
                  (lang-mode (org-src-get-lang-mode lang))
                  (my-buffer (current-buffer)))

              ;; this is the meat of the logic from org-mode with some minor changes to only affect font-lock-face
              ;; since comint has it's own font-lock mode which will override any 'face props we put in
              (when (fboundp lang-mode)
                ;; for debugging purposes having a real (hidden) buffer to see what's being fontified is nice (org does this)
                ;; but we can easily switch to just with-temp-buffer if we no longer want this
                (with-current-buffer (get-buffer-create (format " *amazonq fontify buffer:%s*" lang-mode))
                  (erase-buffer)
                  (insert string " ")
                  (funcall lang-mode)
                  (font-lock-ensure)
                  (let ((pos (point-min)) next)
                    (while (setq next (next-property-change pos))
                      (let ((new-prop (get-text-property pos 'face))) ;; we have to set font-lock-face to the value of face (i.e. font-lock-function-face)
                        (put-text-property (+ block-start (1- pos)) (1- (+ block-start next)) 'font-lock-face new-prop my-buffer))
                      (setq pos next))
                    ))
                )))
          ;; this t is so we continue looking for more blocks in case the output had multiple src blocks
          t)))))

(defun amazon-q--comint-trigger-fontification (_)
  "Intended to be used as a `comint-output-filter-function'.
Checks `amazon-q--comint-ready-for-input' before running `amazon-q--comint-fontify-src-blocks'."
  (when amazon-q--comint-ready-for-input (amazon-q--comint-fontify-src-blocks)))


(define-derived-mode amazon-q-comint-mode comint-mode "Amazon Q"
  "Major mode for Amazon Q chat sessions with the comint backend."
  (add-hook 'comint-output-filter-functions #'amazon-q--comint-trigger-fontification nil t)

  ;; need to unset the accumulated prompt output in the case where the user is interacting with amazon q
  ;; outside of amazon-q--comint-send
  (add-hook 'comint-input-filter-functions
            (lambda (_)
              (setq amazon-q--comint-ready-for-input nil)
              (setq amazon-q--comint-accumulated-prompt-output ""))
            nil t))


(provide 'amazon-q-comint-backend)
;;; amazon-q-comint-backend.el ends here.
