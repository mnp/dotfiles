(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

(defun most-recent-mode-buffer (mode buffs)
  "search list of buffers and return most recently accessed mode buffer"
  (cond ((null buffs) nil)
	((equal mode (buffer-mode (car buffs))) (car buffs))
	(t (most-recent-mode-buffer mode (cdr buffs)))))

(defun switch-to-most-recent-org-buffer ()
  "if in org mode, jump to most recent other buffer, otherwise jump to most recently accessed org-mode buffer"
  (interactive)
  (if (equal major-mode 'org-mode) 
      (switch-to-buffer nil)
    (switch-to-buffer (most-recent-mode-buffer 'org-mode (buffer-list)))))

(defun my-org-mode-hook ()
    (setq fill-column 99
	  org-export-with-toc nil    ;; do not generate a TOC on export please
  	  org-agenda-text-search-extra-files (directory-files my-org-dir t "org$")
	  org-refile-targets '((my-notes-orgfile  . (:level . 1))))
    (auto-fill-mode 1))

(setq org-capture-templates
      '(
	("j" "Journal entry with date" plain
         (file+datetree+prompt my-journal-dir)
         "* "
         :unnarrowed t)

	("t" "Quick task" entry
         (file+headline  "Tasks")
         "* TODO %^{Task}"
         :immediate-finish t)

	("T" "Full task" entry
         (file+headline my-task-orgfile "Tasks")
         "* TODO "
         :unnarrowed t)

	("n" "Quick note" item
	 (file+headline my-notes-org "Quick notes"))))

