;;; pakcage --- Summary
;;; Commentary:
;; 
;;; Code:
;; requiring libraries
(require 'org)
(require 'org-ml)
(require 'dash)



;; vars
(defvar dedicated-session-in nil
  "state of whether now a dedicated session has been started")

(defvar dedicated-session-topic nil
  "the topic of current dedicated session")

(defvar dedicated-session-journal nil
  "the journal of dedicated session. logs goes into it")

(defvar deidcated-session-source nil
  "the source of prompts/rest methods of dedicated session")

(defvar dedicated-session-current-start-time nil
  "the start time of thsi session. used in the journal headline to make it unique.")

(defvar dedicated-session-journal-plist '(:start-time "start-time"
					      :end-time "end-time"
					      :duration "duration")
  "the properties used by journal entries. Used to keep track of the properties(and avoid typo)")

(defvar dedicated-session-default-dumping-prompt-fmt
  "This stage you dump everything irrelevent to %s out of your brain.\n"
  "the default dumping prompt format used in the dumping buffer")

(defvar dedicated-session-bar/rest-number 0
  "mark the nth bar/rest. make the heading in journal unique."
  )


;; test environment
(setq dedicated-session-journal "~/playground/projects/dedicated-session-journal.org")
(setq dedicated-session-source "./dedicated-session-source.org")
;; end of test environment

;; some org-edit-util functions
;; will be a standalone library if too many are defined
(defun hermanhel/org-find-headline-in-buffer-olp  (path)
  "(hermanhel/org-find-headline-in-buffer-olp \"heading1\" \"heading1.1\") would move point to the start of heading1.1 heading under heading1 subtree. If at heading1 have no child, will insert heading1.1 inside heading1 subtree. If heading1 have child but no heading1.1, will insert heading1.1 at the end of heading1 subtree.`(interactive)'
future could make the direction be customizable"
  (interactive)
      (goto-char (point-min))
    
      (while (and (not (equal (length path) 0)))
        (let ((cur-finding (car path)))
          (cond
       ;; if buffer have no heading
       ((not (org-ml-this-buffer-has-headlines))
        ;;insert cur-finding heading at end of buffer
        (goto-char (point-max))
        (org-insert-heading)
        (insert cur-finding));;and search again(another round of while loop)
       ;; if buffer have headings, and
       ;; now not on a heading(at the beginning of program)
       ((not(org-on-heading-p))
        ;;goto next heading
        (org-next-visible-heading 1))
       ;; if the buffer have heading and now on a heading
       ;; and at top level and this level have no headings same as cur-finding(last node and is not)
       ((and (equal (nth 1 (org-heading-components)) 1)
             (equal (save-excursion
                      (outline-end-of-subtree)
                      (point))
                    (point-max))
             (not (equal (nth 4 (org-heading-components)) cur-finding)))
        ;; insert a heading at end
        (org-insert-heading-respect-content)
        (insert cur-finding))
       ;;  buffer have heading, now on a heading,
       ;; and not on top, last node, and is not
       ((and (> (nth 1 (org-heading-components)) 1)
             (equal (save-excursion
                      (outline-end-of-subtree)
                      (point))
                    (save-excursion
                      (outline-up-heading 1)
                      (outline-end-of-subtree)
                      (point)))
             (not (equal (nth 4 (org-heading-components)) cur-finding)))
        ;;insert at same level at end
        (org-insert-heading-respect-content)
        (insert cur-finding)
    
        )
       ;;buffer have heading, now on a heading,
       ;;find a match, cur-finding is the last in path
       ((and (equal (nth 4 (org-heading-components)) cur-finding)
             (equal (length path) 1)
             
        )
        (setq path (cdr path)))
    
       ;; buffer have heading, now on a heading,
       ;; and find a match, and have no subheadings
        ((and (equal (nth 4 (org-heading-components)) cur-finding)
              (not (outline-has-subheading-p)))
        ;; insert a subheading with next heading in path
         (end-of-line)
         (org-insert-heading)
        (org-do-demote)
        (insert (nth 1 path))
        ;; go back a level, to maintain the consistency of end state in this cond.
        (setq path (cdr path)
        ))
       ;; buffer have heading, now on a heading,
       ;; and find a match, and have subheadings
       ((and (equal (nth 4 (org-heading-components)) cur-finding)
             (outline-has-subheading-p))
        ;;goto next level, shrink path to search next heading
        (org-next-visible-heading 1)
        (setq path (cdr path))
        )
       ;; buffer have heading, now on a heading, in the middle, not finding it,
       ;; not any above
       (t
        (org-forward-heading-same-level 1))
       ))))

;;helper functions
(defun dedicated-session-find-current-entry-in-journal ()
  "find current entry in journal file. create new entry if necessary.
Entry-types could be: 'dumping, 'doing/bar, 'doing/rest, 'releasing, corresponding to dumping, bar and rest heading under doing, and releasing heading in the current session's entry.
Empty entry-type would put point at start of session entry.
other entry-type would result in user-error (this may be changed in futureversions to provide a customizable process design.

The funciton assume that it's already in a buffer of journal file.

usage:
(with-temp-buffer (insert-file-contents dedicated-session-journal)
 (org-mode) (dedicated-session-find-current-entry-in-journal))"
  (let* ((entry-type dedicated-session-in)
	 (headline-string (cond ((equal 'dumping entry-type) '("dumping"))
			       ((equal 'doing/bar entry-type) '("doing" (format  "bar %d" (floor (/ dedicated-session-bar/rest-number 2)))))
			       ((equal 'doing/rest entry-type) '("doing" (format "rest %d" (floor (/ dedicated-session-bar/rest-number 2)))))
			       ((equal 'releasing entry-type) '("releasing"))
			       ))
	 (olp (list (format-time-string "%Y" dedicated-session-current-start-time)
		    (format-time-string "%B" dedicated-session-current-start-time)
		    (format-time-string "%d" dedicated-session-current-start-time)
		    (format "%s Session: %s"
			    (format-time-string "%H:%M"
						dedicated-session-current-start-time)
			    dedicated-session-topic
			    ))))
    (hermanhel/org-find-headline-in-buffer-olp (append olp headline-string))    
    ))




;;set properties in the journal
(defun dedicated-session-journal-set-property (property)
  ;;use plist to avoid illegel input
  (let ((property-string (plist-get dedicated-session-journal-plist property)))
    (with-temp-buffer
      (insert-file-contents dedicated-session-journal)
      (org-mode)
;;; find the current entry position
      ;;(format-time-string "%Y" (org-current-time))
      (dedicated-session-find-current-entry-in-journal)
      
      (cond
       ((-contains? '(:start-time :end-time) property)
	(org-set-property property-string (format-time-string "<%Y-%m-%d %H:%M>" (org-current-time))))
       ((equal :duration property)
	(org-set-property property-string
			  (format-time-string
			   "%H:%M"
			   (time-subtract 
			    (org-time-string-to-time (org-ml-get-property "start-time" (org-ml-parse-this-headline)))
			    (org-time-string-to-time (org-ml-get-property "end-time" (org-ml-parse-this-headline))))))
	)    
       (write-region (point-min) (point-max) dedicated-session-journal)
       ))))



;;minor mode dedicated-session-dumping
(define-minor-mode dedicated-session-dumping-mode
  "a minor mode for the dumping buffer. Mainly just to set C-c C-c keybinding."
  :lighter " dumping"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") 'dedicated-session-dumping-finalize)
	    map))
(defun dedicated-session-dumping-finalize ()
  "close the dumping buffer and enter doing stage"
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun dedicated-session-read-source (thing)
  "thing could be: 'rest-methods."
  (setq read-result '())
  
  (let ((current-tag (cond ((equal dedicated-session-in 'dumping) "dumping")
			   ((equal dedicated-session-in 'doing/rest) "doing")
			   ((equal dedicated-session-in 'releasing) "releasing")
			   )
		     )
	)
   (with-temp-buffer
     (insert-file-contents dedicated-session-source)
     (org-mode)
     (setq buffer (org-ml-parse-this-buffer))
     (setq tops (org-ml-headline-get-subheadlines buffer))
     (hermanhel/org-find-headline-in-buffer-olp '("rest methods"))     
     (if (outline-has-subheading-p)
	 (progn (org-next-visible-heading 1)
		(while (not (equal (save-excursion (org-forward-heading-same-level 1) (point)) (point)))
		  
		  ;;(message (nth 4 (org-heading-components)))
		  (message current-tag)
		  (when (member current-tag (org-get-tags))
		     (add-to-list 'read-result (nth 4 (org-heading-components)))
		      (add-to-list 'read-result (org-get-entry)))
		  (org-forward-heading-same-level 1)))
       (user-error "rest methods have no methods to show. Add at least one in %s" dedicated-session-source))
     read-result
     )))
(member "dumping" '("dumping" "releasing"))




;;entry function
(defun dedicated-session ()
  "entry point of dedicated session."
  (interactive)
  (setq dedicated-session-in t)
  (dedicated-session-dumping)
  (message "dedicated session started!!!")
  )

;;dumping function
(defun dedicated-session-dumping ()
  "start dumping stage"
  ;;; set topic
  (setq dedicated-session-topic
	 (read-string "topic:")	
	 )
  (setq dedicated-session-in 'dumping)
  (setq dedicated-session-current-start-time (org-current-time))
  ;;; open journal file
  (dedicated-session-journal-set-property :start-time)
  (switch-to-buffer "*dedicated-session*")
  (org-mode)
  (dedicated-session-dumping-mode)
  (insert "* Dumping\n")
  (insert (format dedicated-session-default-dumping-prompt-fmt dedicated-session-topic))
  (org-return)
  (setq rest-methods (dedicated-session-read-source 'rest-methods))
  (while rest-methods
    (insert "** ")
    (insert (car (last rest-methods)))
    (org-return)
    (setq rest-methods (butlast rest-methods))
    (insert (car (last rest-methods)))
    (org-return)
    (setq rest-methods (butlast rest-methods)))
  )


;;doing functions
;;doing functions are called 

(defun dedicated-session-doing ()
  "start doing stage"
  (setq dedicated-session-in 'doing)
  ;; start a bar
  (dedicated-session-bar-rest-toggle)
  )
(defun dedicated-session-bar-rest-toggle (&optional done?)
  "toggle between bar and rest in doing stage"
  (unless (member dedicated-session-in '(doing doing/bar doing/rest))
    (user-error "you must toggle between bar and rest in doing stage"))
  ;;log end-time
  (if (equal dedicated-session-in 'doing)
      nil
    (progn (dedicated-session-journal-set-property :end-time)
	   (dedicated-session-journal-set-property :duration)))
  ;;toggle state
  (setq dedicated-session-in (cond ((member dedicated-session-in '(doing doing/rest))
				    'doing/bar)
				   ((equal dedicated-session-in 'doing/bar)
				    'doing/rest)))
  (if (not done?)
      (progn
       ;;inc the bar number
       (setq dedicated-session-bar/rest-number (incf dedicated-session-bar/rest-number))
       ;;log start-time. if done? then not log start-time
       (dedicated-session-journal-set-property :start-time)
       (when (equal dedicated-session-in 'doing/rest)
	 (dedicated-session-doing-rest-prompt)))
    nil)
  
  
  )



(provide 'dedicated-session)
;;; dedicated-session.el ends here
