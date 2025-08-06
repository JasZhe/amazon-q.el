;;; amazon-q.el --- Comint backend for amazon q -*- lexical-binding: t; -*-
;;;
;;; Currently based on comint, but potentially a real terminal emulator backend might work better..
(defun amazon-q--get-buffer-create ()
  (when (project-current)
    (get-buffer-create (concat "*amazon-q" (project-name (project-current))))))

(defun amazon-q--send (prompt)
  (setq amazon-q--term-ready nil)
  (term-send-string (get-buffer-process (amazon-q--get-buffer-create)) (format "%s\r" prompt)))


(defvar amazon-q--term-ready nil)

(defun amazon-q--process-filter (proc string)
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
  (term-emulate-terminal proc string)
  )

(defun amazon-q--start ()
  (interactive)
  (let* (;; this is so amazon q cli will send us bells if chat notifications are enabled.
         ;; we can then use the bell character as a way to check if aamzon q is ready to accept more prompts
         (term-term-name "xterm-256color")
         (process-environment (cons "EDITOR=emacsclient" process-environment))
         (buffer (amazon-q--get-buffer-create)))
    (with-current-buffer buffer
      (amazon-q-term-mode)
      (term-exec buffer "q-chat" "q" nil nil)
      (amazon-q--send "q settings chat.enableNotifications true") ;; this is useful for knowing when the process is ready to accept more input
      (set-process-filter (get-buffer-process (current-buffer)) 'amazon-q--process-filter))
    (switch-to-buffer buffer)))


(defvar amazon-q-term-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-j>") #'term-send-raw)
    map))

(define-derived-mode amazon-q-term-mode term-mode "Amazon Q Term"
  :keymap amazon-q-term-mode-map)

;; just my stuff cause doom
(map! :map amazon-q-term-mode-map :i "<C-j>" #'term-send-raw)
(map! :i "C-j" nil)

(defvar amazon-q--tool-requiring-permission "" "Potential tool that might require permission from the user")

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

(defun amazon-q--send-region (arg)
  "Send the current region to amazon Q.
With a prefix arg, edit the prompt before sending."
  (interactive "P")
  (setq amazon-q--send-region-immediately (not arg))
  (setq amazon-q--buffer (amazon-q--get-buffer-create))
  (setq amazon-q--text-to-send (buffer-substring-no-properties (region-beginning) (region-end)))
  (amazon-q--send "/editor"))


(defun amazon-q--clear-context ()
  "Runs /clear to clear amazon Q context."
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (amazon-q--send "/clear")
  (amazon-q--send "y"))


(defun amazon-q--compact-context ()
  "Runs /compact to compact amazon Q context."
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (amazon-q--send "/compact"))


(defun amazon-q--add-file-to-context ()
  "Prompt for a file to add to amazon Q context."
  (interactive)
  (display-buffer (amazon-q--get-buffer-create))
  (let ((file-to-add (read-file-name "Add to context: ")))
    (amazon-q--send (format "/context add %s" file-to-add))))
