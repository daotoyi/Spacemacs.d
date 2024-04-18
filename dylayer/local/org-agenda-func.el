;;; org-agenda-func.el
;;; Commentary:
;;; Code:


;;; mobile sync
(defvar org-mobile-sync-timer nil)
;; (defvar org-mobile-sync-idle-secs (* 60 10))
(defvar org-mobile-sync-idle-secs (* 60 60))
(defun org-mobile-sync ()
  "enable mobile org idle sync"
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))

(defun org-mobile-sync-enable ()
  "enable mobile org idle sync"
  (interactive)
  (setq org-mobile-sync-timer
        (run-with-idle-timer org-mobile-sync-idle-secs t
                             'org-mobile-sync)))

(defun org-mobile-sync-disable ()
  "disable mobile org idle sync"
  (interactive)
  (cancel-timer org-mobile-sync-timer))

(if (eq system-type 'darwin)
    (org-mobile-sync-enable))


;;; org-agenda-view
(setq org-agenda-custom-commands
      '(("w" . "TaskScheme")
        ;; ("wa" "     important &&     urgent" tags-todo "+PRIORITY=\"A\"")
        ("wb" "     important && not urgent" tags-todo "+2Quadrant")
        ;; ("wc" " not important &&     urgent" tags-todo "+period")
        ;; ("wb" "    important && not urgent" tags-todo "-weekly-monthly-daily+PRIORITY=\"B\"")
        ;; ("wc" "not important && not urgent" tags-todo "+PRIORITY=\"C 

        ("g" "GTD View"
	       ((stuck "") ;; review stuck projects as designated by org-stuck-projects
          (tags-todo "Inbox")
          ;; (tags-todo "Context")
          (tags-todo "Waiting")
          ;; (tags-todo "Project")
          (tags "Someday")
          ;; (tags "Reference")
          ;; (tags "Transh")
          ))

	       ("v" . "Agenda view")
         ;; ("vp" "Agenda for all projects"
	       ;;  tags "All Project"
         ;;  ((org-agenda-skip-function 'dy/org-agenda-skip-only-timestamp-entries)
         ;;   (org-agenda-overriding-header "Agenda for all projects: "))
	       ;;  "e:/Refine/GTD/exportview.html")   ;; org-store-agenda-views - export files.
	       ;; ("vt" "Agenda view for all finished tasks"
	       ;;  todo "DONE|CANCELED"
	       ;;  ((org-agenda-skip-function 'dy/org-agenda-skip-unfinished-entries)
         ;;   (org-agenda-overriding-header "All finished tasks: "))
         ;;  "e:/Refine/GTD/exportview.org")
	       ("vu" "ITEMs unscheduled"
	        alltodo ""
          ((org-agenda-skip-function 'dy/org-agenda-skip-scheduled-entries)
           (org-agenda-overriding-header "ITEMs unscheduled: "))
          org-mobile-exportview-file ;; "~/gtd/exportview.org"
          )
         ("vd" "ITEMs no deadlines"
	        alltodo ""
          ((org-agenda-skip-function 'dy/org-agenda-skip-not-deadline-entries)
           (org-agenda-overriding-header "ITEMs no deadlines: "))
          org-mobile-exportview-file)
         ))

;; Skip entries which only have timestamp but no TODO keywords.
(defun dy/org-agenda-skip-only-timestamp-entries ()
  (org-agenda-skip-entry-if 'nottodo 'any))

;; Skip entries which are not finished.
(defun dy/org-agenda-skip-unfinished-entries ()
  ;; (org-agenda-skip-entry-if 'nottodo '("DONE")))
  (org-agenda-skip-entry-if 'todo 'any))

;; Skip unscheduled entries.
 (defun dy/org-agenda-skip-scheduled-entries ()
   (org-agenda-skip-entry-if 'timestamp
			     'todo '("DONE" "WAITING" "CANCELED")
			     'regexp ":Project:"))

;; Skip entries which are not deadlines.
 (defun dy/org-agenda-skip-not-deadline-entries ()
   (org-agenda-skip-entry-if 'notdeadline))

;; used by org-clock-sum-today-by-tags
(defun filter-by-tags ()
   (let ((head-tags (org-get-tags-at)))
     (member current-tag head-tags)))


;; defun function to show spend clock for each [sub]task
(defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("Inbox" "Project" "Someday" "Reference"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
        (insert output-string))
    output-string))

(global-set-key (kbd "C-c C-x t") 'org-clock-sum-today-by-tags)


;;; org-pomodoro
;;; remove to dylayer/package#init-org-pomodoro.
;; C-c C-x C-i :start clock（donnot show on spaceline）/ clock in
;; C-c C-x C-o :stop clock                             / clock out
;; C-c C-x C-r :make dynamic table
;; C-c C-x C-r :refresh manual 

;; (global-set-key "\C-xps" 'org-pomodoro)                       ;; start org-pomodoro
;; (global-set-key "\C-xpv" 'spaceline-toggle-org-pomodoro-off)  ;; turn-off org-pomodoro
;; (global-set-key "\C-xpk" 'org-pomodoro-kill)                  ;; stop?
;; (global-set-key "\C-xpx" 'org-pomodoro-extend-last-clock)     ;; stop
;; (use-package org-pomodoro
;;   :config
;;   (setq org-pomodoro-audio-player "mplayer")
;;   (setq org-pomodoro-finished-sound-args "-volume 0.7")
;;   (setq org-pomodoro-long-break-sound-args "-volume 0.7")
;;   (setq org-pomodoro-short-break-sound-args "-volume 0.7")
;;   (setq org-pomodoro-ticking-sound-args "-volume 0.3")
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-func)
