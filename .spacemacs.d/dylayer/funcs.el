;;; functions for dylayer.
;; --------------------------------------------------------------------------------------------
(defun dy/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; --------------------------------------------------------------------------------------------
(defun dy/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp"
            "lisp"
            "org"
            "latex"
            "python"
            "C"
            "C++"
            "shell"
            "css"
            "ditaa"
            "sqlite"
            "js"
            ;; "java"
            ;; "dot"
            ;; "gnuplot"
            ;; "R"
            ;; "sql"
            ;; "matlab"
            ;; "perl"
            ;; "ruby"
            )))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(add-hook 'org-mode-hook '(lambda ()
                            ;; turn on flyspell-mode by default
                            (flyspell-mode 1)
                            ;; C-TAB for expanding
                            (local-set-key (kbd "C-<tab>") 'yas/expand-from-trigger-key)
                            ;; keybinding for editing source code blocks
                            (local-set-key (kbd "C-c s e") 'org-edit-src-code) ; sys cmd.
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i") 'dy/org-insert-src-block)
                            (global-set-key "\C-csI" 'org-insert-structure-template)
                            ))

;; --------------------------------------------------------------------------------------------
(defun dy/org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date))
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "(闰月)")))
         (cn-day-string (aref cal-china-x-day-name
                              (1- cn-day))))
    (format "%04d-%02d-%02d 周%s %s%s" year month
            day dayname cn-month-string cn-day-string)))

;; --------------------------------------------------------------------------------------------
;; occur command not work.
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE" 
  (interactive)
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list)
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
      buffer-mode-matches)))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; --------------------------------------------------------------------------------------------
;; set in dy/packages#evil 
(defun dy/maybe-exit-notwork ()
  (with-eval-after-load 'evil
    (evil-define-command maybe-exit ()
      "Exit from insert to normal in org-mode."
      :repeat change
      (interactive)
      (let ((modified (buffer-modified-p)))
        (insert "j")
        (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                               nil 0.5)))
          (cond
           ((null evt) (message ""))
           ((and (integerp evt) (char-equal evt ?j))
            (delete-char -1)
            (set-buffer-modified-p modified)
            (push 'escape unread-command-events))
           (t (setq unread-command-events (append unread-command-events
                                                  (list evt))))))))))

;; --------------------------------------------------------------------------------------------
(defconst my-Ddisk-path "d:/" "My Ddisk dir")
(defun dy/goto-Ddisk-dir ()
  (interactive)
  (dired my-Ddisk-path))

(defconst my-Edisk-path "e:/" "My Edisk dir")
(defun dy/goto-Edisk-dir ()
  (interactive)
  (dired my-Edisk-path))

(defconst my-notes-path "e:/Nutstore/ObsidianDB/" "My Notes(obsidian) dir")
(defun dy/goto-notes-dir ()
  (interactive)
  (dired my-notes-path))

(defconst my-refine-path "e:/refine/" "My refine dir")
(defun dy/goto-refine-dir ()
  (interactive)
  (dired my-refine-path))

(defconst my-blog-path "e:/Refine/GithubPages/OrgBlog/" "My Blog dir")
(defun dy/goto-blog-dir ()
  (interactive)
  (dired my-blog-path))
;; --------------------------------------------------------------------------------------------
(defun dy/recursive-grep ()
  "Recursively grep file contents.  `i` case insensitive; `n` print line number;
`I` ignore binary files; `E` extended regular expressions; `r` recursive"
  (interactive)
  (let* ((search-term (read-string "regex:  "))
         (search-path
          (directory-file-name (expand-file-name (read-directory-name "directory:  "))))
         (default-directory (file-name-as-directory search-path))
         (grep-command
          (concat
           grep-program
           " "
           "-inIEr --color=always -C2"
           " "
           search-term
           " "
           search-path)))
    (compilation-start grep-command 'grep-mode (lambda (mode) "*grep*") nil)))

;; --------------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------------
;; org-hugo capture
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "** TODO " title "     :@Inbox:") ;after "@", identified category
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (setq org-blog-posts (expand-file-name "E:/Refine/GithubPages/OrgBlog/post-box.org" refine-directory))
  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry (file+olp "E:/Refine/GithubPages/OrgBlog/blog-capture.org" "BlogInbox")
                 (function org-hugo-new-subtree-post-capture-template)
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!

                 ;; ("hi" "Invest" entry (file+olp "E:/Refine/daotoyi.github.io/OrgBlog/invest.org" "Invest")
                 ;;  (function org-hugo-new-subtree-post-capture-template) :clock-in t :clock-resume t)
                 ;; ("hr" "ReadNote" entry (file+olp "E:/Refine/daotoyi.github.io/OrgBlog/readnote.org" "ReadNote")
                 ;;  (function org-hugo-new-subtree-post-capture-template) :clock-in t :clock-resume t)
                 )))

;; --------------------------------------------------------------------------------------------
(defvar blog-timestamp-format "%Y-%m-%dT%H:%M:%S+08:00"
  "Format of function %Y-%m-%d %H:%M will produce something of the form YYYY-MM-DD HH:MM, Do Ch f on `format-time-string' for more info")
(defun dy/insert-blog-time-timestamp ()
  (interactive)  ; permit invocation in minibuffer
  (insert (format-time-string blog-timestamp-format (current-time))))

(defun dy/insert-current-time-timestamp ()
  (interactive)  ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M:%S" (current-time))))

(defun dy/insert-current-date-timestamp ()
  (interactive) ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d" (current-time))))

