(require 'comint)
(defgroup root-inferior nil
  "Running ROOT from Emacs."
  :group 'root)
(defcustom *root-hist* nil
  "Path to Inferior ROOT history file, if nil defaults to \"~/.root_hist\"")

(defcustom *root-sys* (getenv "ROOTSYS")
  "Directory where root lives"
  :type 'string
  :group 'root-inferior)
(defcustom *root-bin* (concat *root-sys* "/bin/root")
  "Path to root binary"
  :type 'string
  :group 'root-inferior)
(defcustom *root-args* ""
  "User defined arguments to get passed to root when the daemon
  is started. '-l' is automatically passed." 
  :type 'string
  :group 'root-inferior)
(defcustom *inf-root-prompt*  "\\(^root\s-\[[0-9]+\]\\)"
  "Regexp for ROOT prompt"
  :type 'regexp
  :group 'root-inferior)
(defcustom inferior-root-buffer "*ROOT Inferior*"
  "Name of buffer for running an inferior ROOT process."
  :type 'string
  :group 'root-inferior)

(defvar root-inferior-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point) map)
  "Basic mode map for `run-root'")

;; This code is inspired by octave-inf.el
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
;;;###autoload
(defalias 'run-root 'inferior-root)

(defun inferior-root-startup ()
  "Start the inferior ROOT process."
  (let ((proc (comint-exec-1 (substring inferior-root-buffer 1 -1)
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

(defun inferior-root-output-filter (proc string)
  "Taken from Octave mode, ring Emacs bell if output starts with
  ASCII bell, pass the rest to `comint-output-filter'."
  (comint-output-filter proc (inferior-root-strip-ctrl-g string)))

(defun inferior-root-strip-ctrl-g (string)
  "Strip ctrl-g from begining of string"
  (when (string-match "^\a" string)
    (ding)
    (setq string (substring string 1))) 
  string)
(defun root-output-filter (output)
  "Filter output from the ROOT process"
  (let ((newline-loc (string-match "\n" output)))
    (substring output 
	       (if newline-loc (+ 1 newline-loc)
		 0)
	     (length output))))
(setq root-repl-keywords
      '(("\.x|\.L|\.class|\.files\.!"	 . font-lock-constant-face)
	("Error.*"			 . font-lock-warning-face)
	("Warning.*"			 . font-lock-negation-char-face)
	("Info.*"	                 . font-lock-doc-face)
	("\\*\\*\\*.*\\*\\*\\*"          . font-lock-negation-char-face)
	("//.*"                          . font-lock-comment-face)))

(defun inferior-root--initialize ()
  "Initialize buffer-local comint variables"
  (setq comint-process-echoes t
	comint-use-prompt-regexp nil))

(define-derived-mode inferior-root-mode comint-mode "root-repl"
  "Major mode for interacting with an inferior ROOT process.

Entry to this mode successively runs the hooks `comint-mode-hook'
and `inferior-root-mode-hook'."

  (let ((local-vars '(comint-prompt-read-only
					;font-lock-defaults
		      )))
    (mapc (lambda (var) (make-local-variable var))
	  local-vars))
  (setq comint-prompt-regexp *inf-root-prompt*
	comint-move-point-for-output 'all
	mode-line-process '(":%s")
	comint-prompt-read-only t
	font-lock-defaults '(root-repl-keywords))
  (ansi-color-for-comint-mode-filter)
  (add-hook 'comint-preoutput-filter-functions 'root-output-filter)
  (add-hook 'inferior-root-mode-hook 'inferior-root--initialize)
  (setq comint-input-ring-file-name
	(or *root-hist* "~/.root_hist")
	comint-input-ring-size 1024)
  (comint-read-input-ring t))

(provide 'root-inf)
