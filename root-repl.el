(require 'comint)
(defgroup root-inferior nil
  "Running ROOT from Emacs."
  :group 'root)

(defcustom *root-sys* (getenv "ROOTSYS")
  "Directory where root lives"
  :type 'string
  :group root-inferior)
(defcustom *root-bin* (concat *root-sys* "/bin/root")
  "Path to root binary"
  :type 'string
  :group 'root-inferior)
(defcustom *root-args* ""
  "User defined arguments to get passed to root when the daemon
  is started. '-l' is automatically passed." 
  :type 'string
  :group 'root-inferior)
(defcustom *inf-root-prompt*
  "\\(^root\s-\[[0-9]+\]\\)"
  :type 'regexp
  :group 'root-inferior)

;; This code is inspired by octave-inf.el
;;;###autoload
(defalias 'root-repl 'inferior-root)

(defun inferior-root (arg)
  "Start an inferior ROOT process, buffer is put into root-repl-mode.
  
Unless ARG is non-nil, switch to this buffer."
  (interactive "P")
  (let ((buffer inferior-root-buffer))
    (get-buffer-create buffer)
    (if (comint-check-proc buffer)
	nil
      (with-current-buffer buffer
	(comint-mode)
	(inferior-root-startup)
	(inferior-root-mode)))
    (unless arg
      (pop-to-buffer buffer))))

(defun inferior-root-startup
  "Start the inferior ROOT process."
  (let ((proc (comint-exec (substring inferior-root-buffer 1 -1)
			   inferior-root-buffer
			   *root-bin*
			   (append (list "-l" *root-args*)))))
    (set-process-filter proc 'inferior-root-output-digest)
    (setq comint-ptyp process-connection-type
	  inferior-root-process proc)
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (set-process-filter proc 'inferior-root-output-filter)
    (run-hooks 'inferior-root-startup-hook)))

(define-derived-mode inferior-root-mode comint-mode "root-repl"
  "Major mode for interacting with an inferior ROOT process.

Entry to this mode successively runs the hooks `comint-mode-hook'
and `inferior-root-mode-hook'."
  (setq comint-prompt-regexp *inf-root-prompt*
	mode-line-process '(":%s"))
  (setq comint-input-ring-file-name
	(or *root-hist* "~/.root_hist")
	comint-input-ring-size 1024)
  (comint-read-input-ring t))
