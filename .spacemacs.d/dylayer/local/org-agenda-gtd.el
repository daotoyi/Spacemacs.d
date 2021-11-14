;;; org-agenda-gtd.el ---- agenda & GTD configuratin..
;;; Commentary:
;;; Code:

;;; org-agenda ---------------------------------------------------------------------------
(defvar org-agenda-dir "" "gtd org files location")
(setq org-agenda-compact-blocks t)    ;; Compact the block agenda view

(when (eq system-type 'windows-nt)
  (setq-default org-agenda-dir "E:/Refine/GTD/")
  (setq org-agenda-files '("E:/Refine/GTD/")))
(when (eq system-type 'gnu/linux)
  (setq-default org-agenda-dir "/mnt/e/Refine/GTD/")
  (setq org-agenda-files '("/mnt/e/Refine/GTD/")))

;;; org-agenda files && directory.
(setq org-agenda-file-task (expand-file-name "task.org" org-agenda-dir))
(setq org-agenda-file-note (expand-file-name "note.org" org-agenda-dir))
(setq org-agenda-file-project (expand-file-name "project.org" org-agenda-dir))
(setq org-agenda-file-calendar (expand-file-name "calendar.org" org-agenda-dir))
;; (setq org-agenda-file-finished (expand-file-name "finished.org" org-agenda-dir))
;; (setq org-agenda-file-canceled (expand-file-name "canceled.org" org-agenda-dir))

(setq org-highest-priority ?A)
(setq org-lowest-priority  ?E)
(setq org-default-priority ?B)
(setq org-priority-faces
      '((?A . (:background "yellow"     :foreground "red"       :weight bold))
        (?B . (:background "DodgerBlue" :foreground "white"     :weight bold))
        (?C . (:background "pink"       :foreground "DarkGreen" :weight bold))
        ;; (?D . (:background "cyan"       :foreground "purple"    :weight bold))
        (?D . (:background "yellow"     :foreground "red"       :weight bold))
        (?E . (:background "green"      :foreground "black"5     :weight bold))
        ))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)   ;; switch between org-mode buffers.(obsolete)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link) ;; setup links anywhere in emacs. && "\C-c \C-l" invoke
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
;; (define-key global-map "\C-cc" 'org-capture)

;;; org-agenda-view ------------------------------------------------------------------------
(setq org-agenda-custom-commands
      '(
        ("w" . "TaskScheme")
        ("wa" "     important &&     urgent" tags-todo "+PRIORITY=\"A\"")
        ("wb" "     important && not urgent" tags-todo "+2Quadrant")
        ("wc" " not important &&     urgent" tags-todo "+period")
        ;; ("wb" "    important && not urgent" tags-todo "-weekly-monthly-daily+PRIORITY=\"B\"")
        ;; ("wc" "not important && not urgent" tags-todo "+PRIORITY=\"C\"")
        ;; ("g" "GTD View"
	;;  (
	  ;; (stuck "") ;; review stuck projects as designated by org-stuck-projects
          ;; (tags-todo "Inbox")
          ;; (tags-todo "Context")
          ;; (tags-todo "Waiting")
          ;; (tags-todo "Project")
          ;; (tags "Someday")
          ;; (tags "Reference")
          ;; (tags "Transh")
          ;; ))
	("v" . "Agenda view")
        ("vp" "Agenda for all projects"
	 tags "All Project"
         (
	  ;; (org-agenda-skip-function 'tjh/org-agenda-skip-only-timestamp-entries)
          (org-agenda-overriding-header "Agenda for all projects: "))
	 "e:/Refine/GTD/exportview.html")   ;; org-store-agenda-views - export files.
	;; ("vt" "Agenda view for all finished tasks"
	;;  todo "DONE|CANCELED"
	;;  (
	;;   ;; (org-agenda-skip-function 'tjh/org-agenda-skip-unfinished-entries)
        ;;    (org-agenda-overriding-header "All finished tasks: "))
        ;;   "e:/Refine/GTD/exportview.org")	
	("vu" "ITEMs unscheduled"
	 alltodo ""
          ((org-agenda-skip-function 'tjh/org-agenda-skip-scheduled-entries)
           (org-agenda-overriding-header "ITEMs unscheduled: "))
          "e:/Refine/GTD/exportview.org")
        ("vd" "ITEMs no deadlines"
	 alltodo ""
         ((org-agenda-skip-function 'tjh/org-agenda-skip-not-deadline-entries)
          (org-agenda-overriding-header "ITEMs no deadlines: "))
         "e:/Refine/GTD/exportview.org")
	))

;; Skip entries which only have timestamp but no TODO keywords.
(defun tjh/org-agenda-skip-only-timestamp-entries ()
  (org-agenda-skip-entry-if 'nottodo 'any))

;; Skip entries which are not finished.
(defun tjh/org-agenda-skip-unfinished-entries ()
  ;; (org-agenda-skip-entry-if 'nottodo '("DONE")))
  (org-agenda-skip-entry-if 'todo 'any))

;; Skip unscheduled entries.
 (defun tjh/org-agenda-skip-scheduled-entries ()
   (org-agenda-skip-entry-if 'timestamp
			     'todo '("DONE" "WAITING" "CANCELED")
			     'regexp ":Project:"))

;; Skip entries which are not deadlines.
 (defun tjh/org-agenda-skip-not-deadline-entries ()
   (org-agenda-skip-entry-if 'notdeadline))

;; used by org-clock-sum-today-by-tags
(defun filter-by-tags ()
   (let ((head-tags (org-get-tags-at)))
     (member current-tag head-tags)))

;; -----------------------------------------------------------------------------------------------
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-agenda-gtd)
;;; org-agenda-gtd.el ends here
