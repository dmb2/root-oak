;;; lab-notebook.el -- Helper functions to publish a lab notebook with
;;; org-mode and jekyll

;; Copyright (C) 2013 David Bjergaard

;; Author: David Bjergaard <dbjergaard@gmail.com>
;; Keywords: log, lab notebook, org-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary: 

;; This is a set of org-mode related functions to integrate org-mode
;; into a lab-notebook style blog system for documenting scientific
;; computing projects.

(defvar *org-project-publish-dir* "~/research-log/")
(defvar *research-log-author* "David Bjergaard")
(defvar *research-log-email* "david.b@duke.edu")
(defun write-archive-file (notebook-archive-fname time post-fname post-title)
  (with-current-buffer (find-file-noselect notebook-archive-fname)
    (goto-char (point-max))
    (insert (format "- %s: [[file:./posts/%s][%s]]\n" time post-fname post-title))
    (save-buffer)))
(defun write-task-list (task-fname)
  (with-current-buffer (find-file-noselect task-fname)
    (goto-char (point-max))
    (yank)
    (save-buffer)))
(defun write-post-file (post-fname post-dir post-title post-content-fname)
  "Write out the header information for this post"
  (with-temp-file (expand-file-name post-fname post-dir)
		    (insert (format "#+TITLE: %s\n" post-title))
		    (insert (format "#+AUTHOR: %s\n" *research-log-author*))
		    (insert (format "#+EMAIL: %s\n" *research-log-email*))
		    (insert "#+INCLUDE: ./post_frontmatter.org org\n")
		    (insert "#+INCLUDE: ./post_header.org org\n")
		    (insert (format "#+INCLUDE: ./%s org\n" post-content-fname))))
(defun strip-date (heading)
  "Return the org heading without the date from org-mode heading"
  (replace-regexp-in-string 
				 "[ \t]*:[\:a-z]*:" "" (replace-regexp-in-string
							"<[[:digit:]\-]* [a-z]*> " "" 
							heading)))
(defun dash-title (title)
  "Strip non-filename characters and replace spaces with dashes"
  (replace-regexp-in-string "[:=\(\)\?]" ""
			    (replace-regexp-in-string
			     "[ \t]" "-" title)))
(defun write-post (tags time archive-fname)
  (let* ((post-title (strip-date (org-get-heading)))
	 (title (dash-title post-title))
	 (str-time (and (string-match "\\([[:digit:]\-]+\\) " time)
			(match-string 1 time)))
	;TODO add a unique id to avoid name collisions
	 (post-content-fname (format "%s-%s-content.org" str-time title))
	 (post-fname (format "%s-%s.org" str-time title))
	 (org-buffer (current-buffer))
	 org-content)
    (org-narrow-to-subtree)
    (setq org-content (buffer-substring (point-min) (point-max)))
    (set-buffer org-buffer) (widen)
    (with-temp-file (expand-file-name post-content-fname post-dir)
      (insert org-content))
    (write-post-file post-fname post-dir post-title post-content-fname)
    (write-archive-file archive-fname time post-fname post-title)
    (get-buffer org-buffer)))
(defun make-archive-fname (notebook-name name)
  "Return the path to an archive of the form `notebook-name'-`name' in `*org-project-publish-dir*'"
  (expand-file-name 
   (mapconcat 'identity `(,(substring notebook-name 0 -4) ,name) "-")  *org-project-publish-dir*))
(defun get-short-fname (fname-path)
  (car (last (split-string fname-path "/"))))
(defun split-project-file ()
  "Split Project file into sub-files and copy them to a specified
  directory for later export to a static website."
  (interactive)
  (save-excursion
    (let* ((notebook-short-name (get-short-fname (buffer-file-name)))
	   (notebook-file (expand-file-name (buffer-file-name)
					    (file-name-directory (buffer-file-name))))
	   (post-dir (expand-file-name "posts" *org-project-publish-dir*))
	   (notebook-archive-fname (make-archive-fname notebook-short-name "archive.org"))
	   (notebook-digest-fname (make-archive-fname notebook-short-name "digest.org"))
	   (notebook-task-fname (make-archive-fname notebook-short-name "tasks.org"))) 
      (mapcar (lambda (x)
		(when (file-exists-p x)
		  (delete-file x))) 
	      (list notebook-archive-fname
		    notebook-digest-fname
		    notebook-task-fname))
      (mapc
       (lambda (top-level)
	 (find-file notebook-file)
	 (goto-char (point-min))
	 (org-global-cycle 1)
	 (outline-next-visible-heading top-level)
	 (org-map-tree
	  (lambda ()
	    (let* ((tags (org-entry-get nil "TAGS"))
		  (time (org-entry-get nil "TIMESTAMP"))
		  (is-classified (if tags (string-match "classified" tags) nil))
		  (is-digest (if tags (string-match "digest" tags) nil))
		  (is-task-list (if tags (string-match "tasks" tags) nil)))
	      (unless is-classified
		(cond (is-task-list (save-excursion (org-copy-subtree)
						    (write-task-list notebook-task-fname)))
		      ((and time is-digest) (write-post tags time notebook-digest-fname))
		      (time (write-post tags time notebook-archive-fname)))
		))))) 
       '(1 2 3))
      (mapcar (lambda (x)
		(when (get-buffer (get-short-fname x)) 
		  (kill-buffer (get-short-fname x)))) 
	      (list notebook-archive-fname
		    notebook-digest-fname
		    notebook-task-fname)))))

