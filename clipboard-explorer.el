;;; clipboard-explorer.el --- Exlopre the clipboard -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions

;; clipboard-explorer is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; clipboard-explorer is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;; To use, say `M-x explore-clipboard' and hit RET on whatever
;; selection looks interesting.

;;; Code:

(require 'cl)

(defun explore-clipboard ()
  "Show the current contents of the selections and the clipboard."
  (interactive)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-explorer-mode)
    (loop for type in '(PRIMARY SECONDARY CLIPBOARD)
	  do (insert (format "%s ->\n" type))
	  (loop for selection across (gui-get-selection type 'TARGETS)
		when (and (not (memq selection '(MULTIPLE DELETE)))
			  (x-get-selection-internal type selection))
		do (insert
		    (propertize
		     (format "    %s\n" selection)
		     'data (list type selection)))))
    (goto-char (point-min))))

(defvar clipboard-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" 'explore-clipboard)
    (define-key map "w" 'clipboard-explorer-copy-as-kill)
    (define-key map "\r" 'clipboard-explorer-show)
    map))

(define-derived-mode clipboard-explorer-mode special-mode "Clipboard"
  "Major mode for listing selection and clipboard contents.

All normal editing commands are switched off.
\\<clipboard-explorer-mode>"
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun clipboard-explorer-show ()
  "Show the contents of the selection under point."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No selection under point"))
    (let* ((selection (cadr data))
	   (elem (x-get-selection-internal (car data) selection)))
      (message "%s" (clipboard-explorer-format elem selection)))))

(defun clipboard-explorer-copy-as-kill ()
  "Show the contents of the selection under point."
  (interactive)
  (let ((data (get-text-property (point) 'data)))
    (unless data
      (error "No selection under point"))
    (let* ((selection (cadr data))
	   (elem (x-get-selection-internal (car data) selection)))
      (with-temp-buffer
	(insert (format "%s" (clipboard-explorer-format elem selection)))
	(message "Copied %s" (buffer-string))
	(copy-region-as-kill (point-min) (point-max))))))

(defun clipboard-explorer-format (elem selection)
  (cond
   ((string-match "^text/" (symbol-name selection))
    (if (and (> (length elem) 2)
	     (= (aref elem 0) 255)
	     (= (aref elem 1) 254))
	;; Somehow the selection is UTF-16 when selecting text in
	;; Firefox.
	(decode-coding-string elem 'utf-16-le)
      ;; But some sources add a nul to the end of the elem.
      (decode-coding-string
       (replace-regexp-in-string (string 0) "" elem)
       'utf-8)))
   ((string-match "^image/" (symbol-name selection))
    (with-temp-buffer
      (insert-image (create-image elem
				  (intern (cadr (split-string
						 (symbol-name selection)
						 "/")))
				  t :max-width 500))
      (buffer-string)))
   (t
    elem)))

(provide 'clipboard-explorer)

;;; clipboard-explorer.el ends here
