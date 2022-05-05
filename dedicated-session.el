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

(defvar dedicated-session-buffer-plist nil
  "plist storing properties used by the buffer actions. switch-buffer is used mostly to pop buffers, in which kill-current-buffer would automatically restore the last use buffer, so no need to store window configurations like org-capture in this stage. Therefore it's temprory not in use."
  )

(defvar dedicated-session-default-dumping-prompt-fmt
  "This stage you dump everything irrelevent to =%s= out of your brain.\n"
  "the default dumping prompt format used in the dumping buffer")

(defvar dedicated-session-default-resting-prompt-fmt
  "rest in the doing session to keep your focus on =%s= sharp.\n"
  "default rest prompt format used in the rest in the doing")

(defvar dedicated-session-default-releasing-prompt-fmt
  "now give away your attention on =%s=, as its spotlight fading"
  "default releasing prompt format used in releasing buffer")

(defvar dedicated-session-bar/rest-number 0
  "mark the nth bar/rest. make the heading in journal unique."
  )

(defvar dedicated-session-log-number-this-session 0
  "how many logs have been taken. used to generate log title. could make (last-log) function to navigate between them using re-expr.")

(defvar dedicated-session-log-capture-template-string nil
  "the string, like \"tt\", associated with your org-capture template for the log template
an example of it could be")

(defvar dedicated-session-effort 0
  "the expected length in time of this session. It's expecting a input like \"3:00\" parsed into minutes.")

(defvar dedicated-session-bar-effort 0
  "the length in time of a bar. in minutes")


;; test environment
(setq dedicated-session-journal "~/playground/projects/dedicated-session/dedicated-session-journal.org")
(setq dedicated-session-source "~/playground/projects/dedicated-session/dedicated-session-source.org")
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
			       ((equal 'doing/bar entry-type) `("doing" ,(format  "bar %d" (ceiling (/ (+ 1 dedicated-session-bar/rest-number ) 2)))))
			       ((equal 'doing/rest entry-type) `("doing" ,(format "rest %d" (ceiling (/ (+ 1 dedicated-session-bar/rest-number ) 2)))))
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
  "property should be in format :end-time, :start-time, which is specified in dedicated-session-journal-plist to avoid illegal input"
  ;;use plist to avoid illegel input
  (let ((property-string (plist-get dedicated-session-journal-plist property)))
    (with-temp-buffer
      (insert-file-contents dedicated-session-journal)
      (org-mode)
;;; find the current entry position
      ;;(format-time-string "%Y" (org-current-time))
      (dedicated-session-find-current-entry-in-journal)
      
      (cond
       ((member property '(:start-time :end-time) )
	(org-set-property property-string (format-time-string "<%Y-%m-%d %H:%M>" (org-current-time))))
       ((equal :duration property)
	(org-set-property property-string
			  (format-seconds
			   "%02h:%02m"
			   (time-subtract 
			    (org-time-string-to-time (org-element-property :END-TIME (org-element-at-point)))
			    (org-time-string-to-time (org-element-property :START-TIME (org-element-at-point)))
			    )))
	))    
      (write-region (point-min) (point-max) dedicated-session-journal)
       )))

;;get properties in the journal
(defun dedicated-session-journal-get-property (property)
  "get property of current entry. the name might need modification"
  (let ((property-string (plist-get dedicated-session-journal-plist property)))
    (with-temp-buffer
      (insert-file-contents dedicated-session-journal)
      (org-mode)
      (dedicated-session-find-current-entry-in-journal)
      (org-element-property (intern (upcase (concat ":" property-string))) (org-element-at-point))
      )
    )
  )


;;minor mode dedicated-session-dumping
(define-minor-mode dedicated-session-resting-mode
  "a minor mode for the dumping buffer. Mainly just to set C-c C-c keybinding."
  :lighter " resting"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") 'dedicated-session-resting-finalize)
	    map))
(defun dedicated-session-resting-finalize ()
  "close the rest promt buffer and: enter doing stage if now in dumping;enter next bar if now in doing/rest, end session if now in releasing"
  (interactive)
  (kill-buffer (current-buffer))
  (cond
   ((equal dedicated-session-in 'dumping)
    (dedicated-session-journal-set-property :end-time)
    (dedicated-session-journal-set-property :duration)
    (dedicated-session-doing))
   ((equal dedicated-session-in 'doing/rest)
    (dedicated-session-bar-rest-toggle))
   ((equal dedicated-session-in 'releasing)
    (progn (dedicated-session-journal-set-property :end-time)
	   (dedicated-session-journal-set-property :duration))
    (dedicated-session-end))
   )
  
  )

(defun dedicated-session-read-source (thing)
  "thing could be: 'rest-methods."
  (setq read-result '())
  
  (let ((current-tag (cond ((equal dedicated-session-in 'dumping) "dumping")
			   ((equal dedicated-session-in 'doing/rest) "doing")
			   ((equal dedicated-session-in 'doing/bar) "doing")
			   ((equal dedicated-session-in 'releasing) "releasing")
			   )
		     )
	)
   (with-temp-buffer
     (insert-file-contents dedicated-session-source)
     (org-mode)
     (setq end-of-subtree? '(t t)) ;;2 time to pop
     (setq buffer (org-ml-parse-this-buffer))
     (setq tops (org-ml-headline-get-subheadlines buffer))
     (hermanhel/org-find-headline-in-buffer-olp '("rest methods"))     
     (if (outline-has-subheading-p)
	 (progn (org-next-visible-heading 1)
		(while  end-of-subtree?
		
		  (message (nth 4 (org-heading-components)))
		  (message current-tag)
		  (when (member current-tag (org-get-tags))
		     (add-to-list 'read-result (nth 4 (org-heading-components)))
		      (add-to-list 'read-result  (org-get-entry)))
		  (org-forward-heading-same-level 1)
		  (when (equal (save-excursion (org-forward-heading-same-level 1) (point)) (point))
		    ;; pop 1 item out of end-of-subtree? everytime meet end. The second time would make it nil
		    (setq end-of-subtree? (cdr end-of-subtree?)))
		  ))
       (user-error "rest methods have no methods to show. Add at least one in %s" dedicated-session-source))
     read-result
     )))





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
	 (read-string "topic: ")	
	 )
  (setq dedicated-session-effort
	(org-duration-to-minutes (read-string "Expected session durtion[0:00]: ")))
  (run-with-timer (* 60 (- dedicated-session-effort 10)) 0 'dedicated-session-timer-up)
  (setq dedicated-session-in 'dumping)
  (setq dedicated-session-current-start-time (org-current-time))
  ;;; open journal file
  (dedicated-session-journal-set-property :start-time)
  (dedicated-session-rest-prompt dedicated-session-default-dumping-prompt-fmt)
  )

(defun dedicated-session-timer-up (&optional final?)
  "behaviour of the dedicated session. The Default now is: 1. when set a timer, set it with menus 10 minutes time. when that timer is up, call `dedicated-session-timer-up' 2. prompt for extend. if nil, start a 10 minute timer with final? t to releasing, if non-nil, set a timer of extended time

the final time (10m) is currently hard coded."
  (cond
   (final? (run-with-timer 600 0 'dedicated-session-releasing))
   (t (let ((extension (string-to-number (read-string "10m remaining. Extend session?[input extend time(m)]: "))))
	(if (equal extension "")
	    (run-with-timer (* 60 extension) 0 'dedicated-session-timer-up)
	  (run-with-timer 600 0 'dedicated-session-timer-up t)))))
  )

;;;time information funcions
;;interactive call for statistics
(defun dedicated-session-time-statistics ()
  "display in minibuffer current session's time information, including effort, elapsed and remaining time of both this current bar/stage and the session "
  (interactive)
  (let ((session-info (format "session started at %s with effort %s, progress %s, elapsed time %s, remaining time %s"
			      (format-time-string
			       "%H:%M"
			       dedicated-session-current-start-time)
			      (format-seconds "%02h:%02m" (* 60 dedicated-session-effort))
			      			      (format "%d%%" (/ (time-to-seconds (dedicated-session-get-elapsed-time "session")) 60 dedicated-session-effort 0.01))
			      (format-seconds "%02h:%02m"
					      (dedicated-session-get-elapsed-time "session"))
			      (format-seconds "%02h:%02m"
					      ;; remaining time return in minuites
					      (* (dedicated-session-get-remaining-time "session") 60))
			      
			      ))
	(bar-info  (format "    bar started at %s with effort %s, progress %s, elapsed time %s, remaining time %s"
			      (format-time-string
			       "%H:%M"
			       (org-time-string-to-time (dedicated-session-get-property :start-time)))
			      (format-seconds "%02h:%02m" (* 60 dedicated-session-bar-effort))
			      (format "%d%%" (/ (time-to-seconds (dedicated-session-get-elapsed-time "bar")) 60 dedicated-session-bar-effort 0.01))
			      (format-seconds "%02h:%02m"
					      (dedicated-session-get-elapsed-time "bar"))
			      (format-seconds "%02h:%02m"
					      ;; remaining time return in minuites
					      (* (dedicated-session-get-remaining-time "bar") 60))
			      
			      ))
	)
    (message "%s
%s" session-info bar-info)
    ))
;;getting elapsed time
(defun dedicated-session-get-elapsed-time (kind)
  "when kind is \"session\", get the elapsed time since session started. if \"bar \" get that since bar started return in time object"
  (if (equal kind "session")
      (time-subtract (org-current-time)
		     dedicated-session-current-start-time)
    (with-temp-buffer
      (insert-file-contents dedicated-session-journal)
      (org-mode)
      (dedicated-session-find-current-entry-in-journal)
      (time-subtract (org-current-time)
		     (org-time-string-to-time
		      (org-element-property :START-TIME (org-element-at-point) )))
      )))
;;getting remaining time
(defun dedicated-session-get-remaining-time (kind)
  "remaining time = effort-elpased time. return in minutes"
  (if (equal kind "session")
      (- dedicated-session-effort (floor (/ (time-to-seconds (dedicated-session-get-elapsed-time "session")) 60)))
    (- dedicated-session-bar-effort (floor (/ (time-to-seconds  (dedicated-session-get-elapsed-time "bar")) 60)))
      )
  )


(defun dedicated-session-in-string ()
  "give string based on the stage now is in"
  (cond
   ((equal dedicated-session-in 'dumping) "dumping")
   ((equal dedicated-session-in 'doing/rest) "rest")
   ((equal dedicated-session-in 'doing/bar) "bar")
   ((equal dedicated-session-in 'releasing) "releasing")
   ))





(defun dedicated-session-rest-prompt (prompt-fmt)
  "display a prompt buffer start with:
* head
prompt(with a optional slot for topic)
and display matched rest methods with:
** rest method 1 matched
descriptions

"
  (switch-to-buffer "*dedicated-session*")
  (org-mode)
  (dedicated-session-resting-mode)
  (setq head (dedicated-session-in-string))
  (insert (format "* %s" head))
  (org-return)
  (insert (format prompt-fmt dedicated-session-topic))
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
  (message "Starting Doing!!!")
  ;; start a bar
  (dedicated-session-bar-rest-toggle)
  )

(defun dedicated-session-set-bar-effort (effort)
  (interactive "sbar-effort[in minutes]: ")
  (setq dedicated-session-bar-effort (string-to-number effort
						       )))

(defun dedicated-session-bar-rest-toggle (&optional done?)
  "toggle between bar and rest in doing stage"
  (interactive)
  (unless (member dedicated-session-in '(doing doing/bar doing/rest))
    (user-error "you must toggle between bar and rest in doing stage"))

  ;;log end-time
  (if (equal dedicated-session-in 'doing)
      nil
    (progn (dedicated-session-journal-set-property :end-time)
	   (dedicated-session-journal-set-property :duration)))
  
  ;;; setting timer
  ;;automatically set bar length when the first bar is over
  (when (and (equal dedicated-session-bar-effort 0)
	     ;; the first bar starting set the bar number to 1, so
	     ;; encountering bar number 1 here means it's toggling from
	     ;; bar 1 and rest 1
	     (equal dedicated-session-bar/rest-number 1))
    (setq dedicated-session-bar-effort (org-duration-to-minutes(dedicated-session-journal-get-property :duration))))
  ;;prompt to set bar length if it's not set. this should only execute when entering bar 1
  (when (equal dedicated-session-bar-effort 0)
    (setq dedicated-session-bar-effort 
	  (org-duration-to-minutes
	   (read-string "Expected length of session[default 0(same as length of first bar)]:"))))

  
  ;;toggle state
  (setq dedicated-session-in (cond
			      (done? 'releasing)
			      ((member dedicated-session-in '(doing doing/rest))
			       'doing/bar)
			      ((equal dedicated-session-in 'doing/bar)
			       'doing/rest)
			      ))
  (if (not done?)
      (progn
       ;;inc the bar number
	(cl-incf dedicated-session-bar/rest-number)
       ;;log start-time. if done? then not log start-time
       (dedicated-session-journal-set-property :start-time)
       (message (concat (dedicated-session-in-string) " " (number-to-string (ceiling (/ (+ 1 dedicated-session-bar/rest-number) 2))) " on " dedicated-session-topic " has started!!!"))
       (when (and (> dedicated-session-bar-effort 0)
	      	  (equal dedicated-session-in 'doing/bar))
	 (run-with-timer (* 60 dedicated-session-bar-effort)
			 0
			 'dedicated-session-doing-bar-timer-up))
       (when (equal dedicated-session-in 'doing/rest)
	 (dedicated-session-doing-rest-prompt)))
    (dedicated-session-journal-set-property :start-time)
    (message "releasing " dedicated-session-topic)))  

;;interactively set effort
(defun dedicated-session-bar-effort-set ()
  (interactive)
  (setq dedicated-session-bar-effort (org-duration-to-minutes (read-string (format "bar-effort[current %s]: " dedicated-session-bar-effort)))))

(defun dedicated-session-doing-bar-timer-up (&optional action)
  "the function that is called when the bar timer is up. If specified, use action function"
  (cond
   ;;if action is non-nil, do action
   (action action)
   ;;if soft timer, do action in soft-acitons
   ;; slot for the cond
   ;;if hard timer, do action in hardtimer
   ;; slot for the cond
   ;;if action is nil, toggle state 
   (t (dedicated-session-bar-rest-toggle))
   ))

(defun dedicated-session-doing-rest-prompt ()
  "display rest prompt similar to dumping"
  (dedicated-session-rest-prompt dedicated-session-default-resting-prompt-fmt)
  (message "resting resting!")
  )

(defun dedicated-session-end ()
  "end the session"
  (setq dedicated-session-in nil)
  (setq dedicated-session-current-start-time nil)
  (setq dedicated-session-topic nil)
  (setq dedicated-session-bar/rest-number 0)
  (setq dedicated-session-log-number-this-session 0)
  (setq dedicated-session-bar-effort 0)
  (setq dedicated-session-effort 0))

;;log function
(define-minor-mode dedicated-session-log-mode
  "minor for dedicated-session-log buffer. mainly just to set C-c C-c keybinding"
  :lighter " log"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-c") 'dedicated-session-log-finalize)
	    map))

(defun dedicated-session-log (&optional summary?)
  "a simple log function popping an org buffer with minimum template.
if call with summary? t, means it's called by dedicated-session-releasing-prompt"
  (interactive)
  (condition-case nil
      (org-store-link '() t) ;; t then will be inserted automatically
    (error nil))
  (switch-to-buffer "*dedicated-session-log*")
  (org-mode)
  (dedicated-session-log-mode)
  (if (member dedicated-session-in '(dumping releasing))
      (insert "****** ")
    (insert "******* ")
    )
  (if summary?
      (insert "summary")
      (insert (format "log #%d" (+ 1 dedicated-session-log-number-this-session))))
  (org-set-property "log-time" (format-time-string "[%Y-%m-%d %R]" (org-current-time)))
  (goto-char (point-max))
  (org-insert-link)
  (org-return)
  )

(setq dedicated-session-in 'doing/rest)

(defun dedicated-session-log-finalize ()
  "store the entry in dedicated-session-journal, and kill the buffer"
  (interactive)
  (goto-char (point-min))
  (if (equal (nth 4 (org-heading-components)) "summary")
      (setq summary? t)
    (setq summary? nil))
  
  (setq text (buffer-string))
  (with-temp-buffer
    (insert-file-contents dedicated-session-journal)
    (org-mode) (dedicated-session-find-current-entry-in-journal)
    (goto-char (org-end-of-subtree))
    (org-return)
    (insert text)
    (write-region (point-min) (point-max) dedicated-session-journal)
    )
  (kill-current-buffer)
  (cl-incf dedicated-session-log-number-this-session)
  ;;when the log is summary, call releasing prompt.
  (when summary?
    (dedicated-session-releasing-prompt))
  )


(defun dedicated-session-releasing ()
  "end the last session, enter summary log buffer, which, after finalized, test if it's a summary. After tested summary positive, it will proceed to releasing prompt buffer, which the releasing process go on."
  (interactive)
  ;(setq dedicated-session-in 'releasing)
  (dedicated-session-bar-rest-toggle t)
  (dedicated-session-log t)
 ; (dedicated-session-releasing-prompt)
  )

(defun dedicated-session-releasing-prompt ()
  "open the releasing buffer"
  (dedicated-session-rest-prompt dedicated-session-default-releasing-prompt-fmt)
  )

;;some interactive functions
(defun dedicated-session-see-topic ()
  "show current topic in minibuffer"
  (interactive)
  (if dedicated-session-in
      (message dedicated-session-topic)
    (message "you're not in a dedicated session")))
(defun dedicated-session-see-stage-in ()
  "show what stage curent in"
  (interactive)
  (with-temp-buffer
    (let ((elasped-time nil))
      (insert-file-contents dedicated-session-journal)
      (org-mode)
      (dedicated-session-find-current-entry-in-journal)
      (setq elasped-time (format-seconds
			   "%02h:%02m"
			   (time-subtract 
			    (org-current-time)
			    (org-time-string-to-time (org-element-property :START-TIME (org-element-at-point)))
			    )))
      (message "currently in %s for %s" (dedicated-session-in-string) elasped-time)
      )
    )
  )

(provide 'dedicated-session)
;;; dedicated-session.el ends here
