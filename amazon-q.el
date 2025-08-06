;;; amazon-q.el --- Comint backend for amazon q -*- lexical-binding: t; -*-
;;;
;;; Currently based on comint, but potentially a real terminal emulator backend might work better..
(defun amazon-q--get-buffer-create ()
  (get-buffer-create (concat "*amazon-q-" (project-name (project-current)))))

(defun amazon-q--start ()
  (interactive)
  (let* ((comint-terminfo-terminal "dumb")
         (process-environment (cons "EDITOR=emacsclient" process-environment))
         (process-connection-type t)
         (project (project-current))
         (project-root (project-root project))
         (buffer (amazon-q--get-buffer-create))
         (proc (start-process "amazon-q" buffer "bash" "-c" "q chat --trust-tools=")))
    (with-current-buffer buffer (amazon-q-mode))
    (switch-to-buffer buffer)
    ))


(defvar amazon-q--tool-requiring-permission "" "Potential tool that might require permission from the user")

(defun amazon-q--process-filter (proc string)
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

  (let ((processed-string string))
    ;; Apply ANSI colors instead of filtering them out
    (setq processed-string (ansi-color-apply processed-string))

    ;; Clean up spacing issues while preserving colors
    (setq processed-string (replace-regexp-in-string "\r" "" processed-string))

    ;; Remove the [2K escape sequence (clear to end of line)
    (setq processed-string (replace-regexp-in-string "\033\\[2K" "" processed-string))

    ;; Clean up excessive whitespace but be more gentle
    (setq processed-string (replace-regexp-in-string "\n\n\n+" "\n\n" processed-string))

    (setq processed-string (replace-regexp-in-string "[ \t]+" " " processed-string))
    (comint-output-filter proc processed-string)))

(define-derived-mode amazon-q-mode comint-mode "Amazon Q"
  "Major mode for Amazon Q chat sessions."
  (setq-local doom-real-buffer-p t)
  (setq-local comint-prompt-regexp "^> ")
  ;; (add-hook 'comint-output-filter-functions 'ansi-color-process-output nil t)
  (setq-local comint-use-prompt-regexp t)
  (ansi-color-for-comint-mode-on)
  (set-process-filter (get-buffer-process (current-buffer)) 'amazon-q--process-filter)
  (setq-local comint-prompt-read-only nil))



;;; SENDING REGION
;;; commentary:
;;; multi line inputs don't really work (from my testing at least) in comint buffers cause C-j isn't being interpreted properly I guess.
;;; The way around it was to hook into the emacsclient buffer creation and send the region there and doing it in a way that's almost invisible to the user.
;;;
;;; Maybe there's an actual way to get the multi-line prompts to work with comint but for now this works


(defvar amazon-q-last-client-buffer nil
  "Last buffer opened via emacsclient.")

(defun amazon-q-track-client-buffer ()
  "Track buffers opened via emacsclient."
  (setq amazon-q-last-client-buffer (current-buffer)))

(defvar amazon-q--text-to-send nil "Text to be sent in the emacsclient buffer for amazon q.")

(defun amazon-q--editor-buffer-insert-region ()
  (when amazon-q--text-to-send
    (insert amazon-q--text-to-send)
    (save-buffer)
    (setq amazon-q--text-to-send nil)
    (server-edit))
  (display-buffer (amazon-q--get-buffer-create)))

(add-hook 'server-visit-hook 'amazon-q-track-client-buffer)
(add-hook 'server-switch-hook 'amazon-q--editor-buffer-insert-region)

(defun amazon-q--send-region ()
  (interactive)
  (setq amazon-q--text-to-send (buffer-substring-no-properties (region-beginning) (region-end)))
  (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "/editor\r"))


(defun amazon-q--clear-context ()
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "/clear\r")
  (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "y\r"))


(defun amazon-q--compact-context ()
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) "/compact\r"))


(defun amazon-q--add-file-to-context ()
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (let ((file-to-add (read-file-name "Add to context: ")))
    (comint-send-string (get-buffer-process (amazon-q--get-buffer-create)) (format "/context add %s\r" file-to-add))))
