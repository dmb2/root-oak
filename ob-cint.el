;;; ob-template.el --- org-babel functions for template evaluation

;; Copyright (C) David Bjergaard

;; Author: David Bjergaard <dbjergaard@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

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

;;; Commentary:

;;; Requirements:
;; root-inf

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language
(require 'root-inf)
;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("cint" . "tmp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:cint '())
(defvar org-babel-cint-eoe-indicator "\'org_babel_eoe\'")
(defvar org-babel-cint-eoe-output "ans = org_babel_eoe")
(defvar org-babel-cint-wrapper-method
  "%s
FILE* fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
")

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:cint' function below.
(defun org-babel-expand-body:cint (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-cint)
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-cint-var-to-cint (cdr pair))))
      vars "\n") "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:cint (body params)
  "Execute a block of Cint code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Cint source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (org-babel-cint-initiate-session (first processed-params)))
         ;; variables assigned for use in the block
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:cint'
         (full-body (org-babel-expand-body:cint
                     body params processed-params))
	 (graphical-out-file (org-babel-cint-graphical-output-file params))
	 (result (org-babel-cint-evaluate
		  session
		  (if graphical-out-file
		      (mapconcat 'identity
				 (list
				  "gROOT->SetBatch()"
				  full-body
				  (format "c1->SaveAs(\"%s\")" graphical-out-file))
				 "\n")
		    full-body)
		  result-type)))
    (if graphical-out-file
	nil
	(org-babel-reassemble-table
	 result
	 (org-babel-pick-name
	  (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	 (org-babel-pick-name
	  (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))

    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;; 
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    ))
(defun org-babel-cint-graphical-output-file (params)
  "Name of file to which ROOT should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:cint (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-cint-initiate-session session params))
  	 (var-lines (org-babel-variable-assignments:cint params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-cint-read-string (string)
  "Strip \\\"s from around cint string."
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(defun org-babel-cint-evaluate-session
  (session body result-type)
  "Evaluate BODY in SESSION."
  (let* ((tmp-file (org-babel-temp-file "cint-"))
	 (full-body
	  (case result-type
	    (output
	     (mapconcat
	      #'org-babel-chomp
	      (list body org-babel-cint-eoe-indicator) "\n"))
	    (value (mapconcat
		#'org-babel-chomp
		(list (format org-babel-cint-wrapper-method
			      body
			      (org-babel-process-file-name tmp-file 'noquote)
			      (org-babel-process-file-name tmp-file 'noquote))
		      org-babel-cint-eoe-indicator) "\n"))))
	 (raw (org-babel-comint-with-output
		    (session
		     org-babel-cint-eoe-output
		     t full-body)
		  (insert full-body) (comint-send-input nil t)))
	 results)
    (case result-type
      (value
       (org-babel-cint-import-elisp-from-file tmp-file))
      (output
       (progn
	 (setq results
	       (cdr (member org-babel-cint-eoe-output
			      (reverse (mapcar
					#'org-babel-cint-read-string
					(mapcar #'org-babel-trim raw))))))
	 (mapconcat #'identity (reverse results) "\n"))))))

(defun org-babel-cint-evaluate-external-process (body result-type)
  "Evaluate BODY in an external cint process."
  (let ((cmd org-babel-cint-shell-command))
    (case result-type
      (output (org-babel-eval cmd body))
      (value (let ((tmp-file (org-babel-temp-file "cint-")))
	       (org-babel-eval
		cmd
		(format org-babel-cint-wrapper-method body
			(org-babel-process-file-name tmp-file 'noquote)
			(org-babel-process-file-name tmp-file 'noquote)))
	       (org-babel-cint-import-elisp-from-file tmp-file))))))

(defun org-babel-cint-evaluate
  (session body result-type)
  "Pass BODY to the ROOT process in SESSION.
If RESULT-TYPE equals 'output then return the outputs of the
statements in BODY, if RESULT-TYPE equals 'value then return the
value of the last statement in BODY, as elisp."
  (if session
      (org-babel-cint-evaluate-session session body result-type)
    (org-babel-cint-evaluate-external-process body result-type)))

(defun org-babel-cint-var-to-cint (var)
  "Convert an elisp var into a string of cint source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-cint-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (message "Implement cint-table-or-string!")
  results)

(defun org-babel-cint-initiate-session (&optional session params)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session (or session "*Inferior ROOT*")))
      (if (org-babel-comint-buffer-livep session) session
	(save-window-excursion
	  (run-root)
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name))))
	  (current-buffer))))))

(provide 'ob-cint)
;;; ob-cint.el ends here
