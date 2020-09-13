;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; extended-insert.el
;;
;; Usage:
;;   (setq my-initials "MNP")
;;   (autoload 'extended-insert "extended-insert" "Gives menu to insert various things." t)
;;   (global-set-key "\^Xi" 'extended-insert)
;;
;;   Then, you can press ``Ctrl-x i'' followed by one of these:
;;	  b: a buffer
;;	  c: result of a shell command
;;	  d: date string
;;	  e: result of a lisp eval
;;	  f: a file
;;	  h: mode specific file header, from directory named by ei-headers-dir
;;	  n: note-signature
;;	  s: selected saluation and mail signature
;;	  x: x selection
;;
;; History:
;;   ?? ??? ??  created mid eighties sometime
;;   05 Jan 98	port to xemacs
;;   12 Mar 98	file header insertion
;;   23 Aug 06	dash and equals
;;   03 Mar 12  special perl header
;;   17 Dec 18  use gui-
;;
;; TODO: hydra
;; TODO: clean up and use the Org template instead
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ei-headers-dir "~/banners"
  "*Directory were headers come from.  Each is in a file named for its mode.")

(defconst ei-salutation-list
  '(("Respectfully") ("Sincerely") ("Thanks") ("Chow") ("Best") ("Best Regards")
    ("Yours") ("Later")))

(defconst ei-beg-inc
  "\n--------------------------- begin inclusion--------------------------------\n")

(defconst ei-end-inc
  "---------------------------- end inclusion---------------------------------\n")

(defun ei-saluted-signature (pfx)
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (default (car (car ei-salutation-list)))
	 (reply  (completing-read
		  (format "Salutation (%s): " default)
		  ei-salutation-list)))
    (save-excursion
      (insert
       "\n"
       (if (string= "" reply) default reply)
       ",\n")
      (insert-file-contents (if pfx "~/.signature-me" "~/.signature")))))

; (defun ei-time-stamp ()
;   "Returns a stamp str."
;   (let ((time (current-time-string)))
;     (format "%s%02d%02d"
; 	    (substring time 22 24)
; 	    (car (cdr (assoc
; 		       (substring time 4 7)
; 		       '(("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4) ("May" 5)
; 			 ("Jun" 6) ("Jul" 7) ("Aug" 8) ("Sep" 9) ("Oct" 1)
; 			 ("Nov" 11) ("Dec" 12)))))
; 	    (string-to-int (substring time 8 10)))))

(defun ei-time-stamp ()
  "Returns a stamp str."
  (let ((time (current-time-string)))
    (format "%s %s %s"
	    (substring time 8 10)
	    (substring time 4 7)
	    (substring time 22 24))))

(defun ei-note-signature ()
  "Insert initials and little date."
  (interactive)
  (insert (ei-time-stamp) " " my-initials))

(defun ei-insert-lisp-eval ()
  (interactive)
  (insert (eval-minibuffer "Insert eval: ")))

(defun ei-insert-date ()
  (interactive)
  (insert (current-time-string)))

(defun ei-insert-header ()
  "Inserts a header into beginning of current buffer.  Looks for mode
name of visited buffer in the directory ei-headers-dir, inserts it,
and replaces @YR@ with current 4 digit year, @FILE@ with ... guess what,
@DATE@ with a date stamp, and @INITIAL@ with my-initials.  Cursor is
left at @CURSOR@, if given."
  (interactive)
    (goto-char (point-min))
    (let* ((file (concat ei-headers-dir "/" mode-name))
	   (text (concat ei-headers-dir "/Text"))
	   (rslt (insert-file-contents
		  (cond ((file-exists-p file) file)
			((file-exists-p text) text)
			(t (error "No header for mode %s in %s." mode-name ei-headers-dir))))))
	(ei-global-replace-string "@DATE@" (ei-time-stamp))
	(ei-global-replace-string "@SIG@" my-initials)
	(ei-global-replace-string "@FILE@" (file-name-nondirectory (buffer-file-name)))
	(ei-global-replace-string "@YR@" (substring (current-time-string) -4))
	(if (search-forward "@CURSOR@" nil t)
	     (replace-match "" t t)
	  (goto-char (cadr rslt)))))

(defun ei-global-replace-string (FROM-STRING TO-STRING)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward FROM-STRING nil t)
      (replace-match TO-STRING t t))))

(defun ei-insert-shell-command (pfx cmd)
  "Execute shell-command upon CMD, leaving result at point."
  (interactive "P\nsShell command: ")
  (save-excursion
    (if pfx
	(progn
	  (insert ei-end-inc)
	  (backward-char (length ei-end-inc))
	  (insert ei-beg-inc)))
    (shell-command cmd t)))

(defun ei-insert-file (pfx file)
  "Wrap insert-file."
  (interactive "P\nfInsert file: ")
  (let ((ifcres (insert-file-contents file)))
    (if pfx
	(save-excursion
	  (insert ei-beg-inc)
	   (goto-char  (+ (point) (length ei-end-inc) (cadr ifcres)))
	   (insert ei-end-inc)))))

(defun ei-perl-header ()
  "make current buffer into a new perl program"
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name)))
    (chmod file 509))			; 0775
  (cperl-mode)
  (ei-insert-header))

(defun ei-insert-x-clipboard ()
  "get and insert x clipboard"
  (interactive)
  (insert (gui-get-primary-selection)))

(defun ei-line-of (ch) (insert (make-string 76 ch) "\n"))
(defun ei-dashes () (interactive) (ei-line-of ?-))
(defun ei-equals () (interactive) (ei-line-of ?=))

(defun ei-box-comment()
  (interactive)
  (let ((line (make-string 78 ?-)))
    (insert
     (format "%s %s\n%s \n%s %s\n" comment-start line comment-start comment-start line)))
  (previous-line 2)
  (move-end-of-line nil))

(defun extended-insert (pfx key)
  "Prompts for a one char arg, then calls fn, inserting at point:
  b: a buffer
  c: result of a shell command
  d: date string
  e: result of a lisp eval
  f: a file
  h: mode specific file header, from directory named by ei-headers-dir
  n: note-signature
  p: perl program header and chmods
  s: selected saluation and mail signature
  x: x selection
  :: (colon) - a box comment
  -: line of dashes
  =: line of equals"
  (interactive
   "P\ncb:buff c:shell-cmnd d:date f:file h:header n:note s:signature x:x-cut-buffer - =")
  (call-interactively
   (car (cdr (assoc key
		    '((?b insert-buffer)
		      (?c ei-insert-shell-command pfx)
		      (?d ei-insert-date)
		      (?e ei-insert-lisp-eval)
		      (?f ei-insert-file pfx)
		      (?h ei-insert-header)
		      (?n ei-note-signature)
		      (?p ei-perl-header)
		      (?s ei-saluted-signature pfx)
                      (?: ei-box-comment)
		      (?- ei-dashes)
		      (?= ei-equals)
		      (?x ei-insert-x-clipboard)))))))

;;; end
