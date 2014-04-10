;; Minimal emacs preferences

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq dot-emacs (expand-file-name "~/.emacs.el")
      dot-elc   (expand-file-name "~/.emacs.elc"))
(add-to-list 'load-path (expand-file-name "~/Elisp"))

;(if (file-newer-than-file-p dot-emacs dot-elc)
;    (byte-compile-file dot-emacs t))

(require 'hideshow)
;(require 'dot-mode)
(require 'bs)
;(require 'color-theme)		; 23
(require 'workgroups)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(autoload 'duplicate-following-line "duplicate-line")
(autoload 'duplicate-previous-line "duplicate-line")
(autoload 'extended-insert "extended-insert")
(autoload 'browse-kill-ring "browsekill")
;(autoload 'flymake-mode "flymake")

(if window-system 
    (progn
      
;24
      (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
      (load-theme 'zenburn)

;23
;      (if (fboundp 'color-theme-charcoal-black)
;	  (color-theme-charcoal-black))
;;      (load-library "solarized-theme")

;23      (defun djcb-zoom (n)
;	"with positive N, increase the font size, otherwise decrease it"
;	(set-face-attribute 'default (selected-frame) :height 
;			    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 

; 24
      (global-set-key (kbd "C-+") '(lambda nil (interactive) (text-scale-increase 1)))
      (global-set-key (kbd "C--") '(lambda nil (interactive) (text-scale-decrease 1)))

      ;; this maybe also good for local terminal, but how do we tell
      ;; that from a remote?
      (setq redisplay-dont-pause t)	

      (mouse-avoidance-mode 'jump)
      (add-hook 'server-visit-hook 
		'(lambda () (raise-frame) (recenter)))
      (mouse-wheel-mode 1)
      (delete-selection-mode 1)
      (global-set-key "\C-z" 'undo)))

;; ------------------------------------------------------
;; Modes
;; ------------------------------------------------------

(require 'erc)

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.perlhack.net" "#gaia")))
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun my-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "#gaia") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.perlhack.net" :port 6667 :nick "mnp" :full-name "mitchell perilstein"))))

(global-set-key (kbd "C-c e") 'my-erc-start-or-switch)

;; use deft to get to org quickly on F9
;; http://jblevins.org/projects/deft/
(when (require 'deft nil 'noerror) 
   (setq
      deft-extension "org"
      deft-directory "~/Org/deft/"
      deft-text-mode 'org-mode)
   (global-set-key (kbd "<f9>") 'deft))

;; org mode
(autoload 'org-mode "org")

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keywords 
      '((sequence "TODO" "WAIT" "DONE")))

(load-library "my-org-mods")
(setq org-hide-leading-stars t)
(setq org-export-with-sub-superscripts nil)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(defun my-find-file-hook ()
  (git-gutter)
  (vc-mode-line (buffer-file-name) 'git)
  )

(add-hook 'find-file-hooks 'my-find-file-hook)

;(add-hook 'find-file-hooks 'dot-mode-on)

(defun my-generic-mode-hook ()
  (auto-fill-mode 1)
;  (hs-minor-mode 1)
  )


(defun my-c-mode-common-hook ()
  (c-set-style "stroustrup")
  (setq fill-column 79)
)

(defun indent-buffer ()
  "Re-indent the whole buffer, untabify, remove trailing whitespace."
  (interactive)
  (let ((c-indentation-style "stroustrup"))
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(add-to-list 'completion-ignored-extensions ".dep")


;; Then do M-x perl-outline-mode after you opened the Perl code
;; file. You can then expand and contract subroutines (with C-c @
;; C-t to contract all, C-c @ C-a to expand all, C-c @ C-d to
;; contract a function, C-c @ C-s to expand it).

(defun perl-outline-mode ()
  "set customized outline minor mode for Perl"
  (interactive)
  (setq outline-regexp 
    "#!.\\|\\(pac\\)kage\\|sub\\|\\(=he\\)ad\\|\\(=po\\)d")
  (outline-minor-mode))


(add-to-list 'interpreter-mode-alist '("ratlperl" . cperl-mode))
(defun my-perl-mode-hook ()
  (load-library "mycperl")
  (cperl-mode)
;  (flymake-mode)
  )

(defun my-xml-mode-hook ()
  (hs-minor-mode 1)
  (local-set-key [f9] 'sgml-expand-element)
  (local-set-key [f10] 'sgml-fold-element))

(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key (kbd "<f11>")  'djcb-full-screen-toggle)


(add-hook 'perl-mode-hook 	'my-perl-mode-hook)
(add-hook 'xml-mode-hook        'my-xml-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-generic-mode-hook)
(add-hook 'html-mode-hook 	'my-generic-mode-hook)
(add-hook 'php-mode-user-hook   'my-generic-mode-hook)
(add-hook 'sh-mode-hook 	'my-generic-mode-hook)
(add-hook 'sql-mode-hook	'my-generic-mode-hook)
(add-hook 'c-mode-hook		'my-generic-mode-hook)
(add-hook 'c-mode-hook		'my-c-mode-common-hook)
(add-hook 'c++-mode-hook	'my-generic-mode-hook)
(add-hook 'c++-mode-hook	'my-c-mode-common-hook)
(add-hook 'java-mode-hook	'my-generic-mode-hook)
(add-hook 'makefile-mode-hook	'my-generic-mode-hook)

;(add-hook 'c-mode-hook		'flymake-mode)


;; ------------------------------------------------------
;; Fun
;; ------------------------------------------------------


; (defvar my-hs-hide nil "Current state of hideshow for toggling all.")

;(defun my-toggle-hideshow-all () 
;  "Toggle hideshow all."
;  (interactive)
;  (setq my-hs-hide (not my-hs-hide))
;  (if my-hs-hide
;      (progn (hs-hide-all) (message "hid"))
;    (progn (hs-show-all) (message "show"))))

(defun my-toggle-selective-display (column)
  "cheap hideshow - http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/"
  (interactive "P")
  (set-selective-display
   (or column ; disable toggle if column was supplied
       (unless selective-display 1))))


;;
;; find-tag (M-.) will load "./TAGS" by default, the first time you use it.
;; This will look in parent dirs up to root for it.
;; 

(defun find-file-upwards (file dir)
  "Look for ./FILE, ../FILE, etc ascending until found, returning it or nil."
  (let* ((dir2 (expand-file-name dir))
	 (f (concat dir2 "/" file)))
    (cond ((string-equal "/" dir2) nil)
	  ((file-exists-p f) f)
	  (t (find-file-upwards file (concat dir2 "/.."))))))

(defadvice find-tag (before find-tags-table () activate)
  "Looks a little harder for tags table the first time."
  (or (get-buffer "TAGS")
      (let ((tagfile (find-file-upwards "TAGS" ".")))
	(if tagfile
	    (visit-tags-table tagfile)
	  (error "Can't find TAGS looking upwards from %s" default-directory)))))


;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; TODO: this should go to most recent buffer if already in *gud*

(defun word-at-point ()
  (save-excursion 
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
	   (begin (progn (skip-syntax-backward "w_") (point))))
      (buffer-substring begin end))))

(defun my-code-search (str)
  (interactive (concat "sSearch for: " (word-at-point)))
  (grep-find str "*.cpp" "."))		;{c,cc,cpp,h
;  (rgrep str "*.cpp" "."))		;{c,cc,cpp,h


(defun my-perldb ()
  "Make, or switch to, a perldb buffer. Switch back if we're already there."
  (interactive)
  (if (and (boundp 'gud-comint-buffer) (buffer-live-p gud-comint-buffer))
      (if (eq (current-buffer) gud-comint-buffer)
	  (switch-to-buffer nil)	  	; there already - switch back to previous
	  (switch-to-buffer gud-comint-buffer)) ; not there, so go there
    (call-interactively 'perldb)))		; not running, start it and go there

(defun my-just-one-white (&optional n)
  "Delete all spaces and tabs around point, leaving one space (or N spaces)."
  (interactive "*p")
  (let ((orig-pos (point)))
    (skip-chars-backward " \t\n\r")
    (constrain-to-field nil orig-pos)
    (dotimes (i (or n 1))
      (if (= (following-char) 32)
	  (forward-char 1)
	(insert 32)))
    (delete-region
     (point)
     (progn
       (skip-chars-forward " \t\n\r")
       (constrain-to-field nil orig-pos t)))))


(defun parent-directory (path)
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

;; quick switch between .c (.cpp) and .h files
; (defun find-ch-file ()
;   (interactive)
;   (let* ((fname (buffer-file-name))
; 	 (ext   (file-name-extension fname))
; 	 (base  (file-name-sans-extension fname)))
;     (cond ((string-equal "c" ext)   (find-file (concat base ".h")))
; 	  ((string-equal "cpp" ext) (find-file (concat base ".h")))
; 	  ((string-equal "cxx" ext) (find-file (concat base ".h")))
; 	  ((string-equal "h" ext) 
; 	   (let ((cfile (concat base ".c"))
; 		 (cxxfile (concat base ".cxx"))
; 		 (cppfile (concat base ".cpp")))
; 	     (cond ((file-exists-p cfile) (find-file cfile))
; 		   ((file-exists-p cxxfile) (find-file cxxfile))
; 		   ((file-exists-p cppfile) (find-file cppfile))
; 		   (t (message "no %s, %s, or %s file"
; 			       cfile cxxfile cppfile)))))
; 	  (t (message "all I know is .h <--> .c/.cpp/.cxx")))))
; 
; ;; todo: local for c/c++
; (global-set-key "\C-x\C-h"	'find-ch-file)

(autoload 'fct/find-file "find-companion-thing" nil t)
(global-set-key "\C-x\C-h" 'fct/find-file)
  
;; remove Ctrl-Ms globally
(fset 'dem [?\M-< ?\M-% ?\C-q ?\C-m return return ?! ?\M-<])


;; ------------------------------------------------------
;; Global Preferences
;; ------------------------------------------------------

(setq my-initials "MNP"
      compilation-scroll-output 1
      show-paren-style 'expression
      show-paren-delay 0
      comment-column 70
      fill-column 79
      eval-expression-print-length 99
      regex-tool-backend 'perl
)

;; see discussion http://www.reddit.com/r/emacs/comments/10rdl2/globalautorevertmode_1/
(global-auto-revert-mode 1)

;; i hate splitting vertically in most cases
(setq split-width-threshold nil)

(auto-fill-mode 1)
(auto-compression-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(show-paren-mode t)

; if we don't use vc saves go much faster
(setq vc-handled-backends nil)

;; hilight current line - kinda annoying
;
; (global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; shift-arrows in addition to "C-x o"
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key [f1] 		'switch-to-most-recent-org-buffer)
(global-set-key [f3]            'my-perldb)
(global-set-key [f4]            'my-code-search)
(global-set-key [f5]   		(lambda () (interactive) (revert-buffer t nil)))
(global-set-key [f6]   		(lambda () (interactive) (switch-to-buffer nil)))
(global-set-key [f7]   		'my-toggle-hideshow-all)
(global-set-key [f12]  		'my-toggle-selective-display)
(global-set-key [home] 		'beginning-of-buffer)
(global-set-key [end] 		'end-of-buffer)

(global-set-key "\M-#" 		'calc)
(global-set-key "\M-p" 		'duplicate-previous-line)
(global-set-key "\M-n" 		'duplicate-following-line)
(global-set-key "\M-g" 		'goto-line)
(global-set-key [?\C-_] 	'help-command)
(global-set-key "\C-h"	 	'backward-delete-char)
(global-set-key "\C-x\C-b" 	(lambda () (interactive) (bs-show 1)))  ;;  'electric-buffer-list)
(global-set-key "\C-x4y" 	'browse-kill-ring)
(global-set-key "\C-xi" 	'extended-insert)
(global-set-key "\C-x\C-k" 	'compile)
(global-set-key "\M-SPC" 	'my-just-one-white)
(global-set-key "\C-cr" 	'align-regexp)

;; ; Here's what I use to 'zoom' text in an Emacs window:
;; 
;; (global-set-key (kbd "M-+") 'text-scale-adjust)
;; (global-set-key (kbd "M--") 'text-scale-adjust)
;; (global-set-key (kbd "M-0") 'text-scale-adjust)
;; 
;; ; Apparently this is part of Emacs 23.


;;
;; full-ack
;;
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;(ack-arguments

;; customize full-ack

;;(defun ack-goto-error ()
;;  "Jump to error approximately near point"
;;  (interactive)
;;  (save-excursion
;;    (if (re-search-backward "^--\\|^$" (point-min) t)
;;	(ack-next-error-function 1 nil))))
;;
;; (define-key ack-mode-map "\C-c\C-c" 'ack-goto-error)
;;(define-key ack-mode-map [mouse-2]  'ack-goto-error)


;;;
;;; MEW
;;; http://www.mew.org/en/info/release/mew_1.html#Overview
;;;
(if (equal (system-name) "dx832503tp.trueposition.com") 	; hack only on desktop
    (progn
      (autoload 'mew "mew" nil t)
      (autoload 'mew-send "mew" nil t)
      (setq mew-name "Mitchell Perilstein") ; (user-full-name)
      ;; (setq mew-user "mperilstein") ; (user-login-name)
      (setq mew-mail-domain "trueposition.com")
      
      (setq mew-smtp-server "mail.trueposition.com")
      (setq mew-proto "%")
      (setq mew-imap-server "mail.trueposition.com")

      ;; Optional setup (e.g. C-xm for sending a message):
      (autoload 'mew-user-agent-compose "mew" nil t)
      (if (boundp 'mail-user-agent)
	  (setq mail-user-agent 'mew-user-agent))
      (if (fboundp 'define-mail-user-agent)
	  (define-mail-user-agent
	    'mew-user-agent
	    'mew-user-agent-compose
	    'mew-draft-send-message
	    'mew-draft-kill
	    'mew-send-hook))

      (setq mew-use-cached-passwd t)
      ))


;; keep this last

(if window-system 
    (progn
      (if (require 'edit-server)
	  (edit-server-start)
	(server-start))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-arguments (quote ("--ignore-dir=unittest")))
 '(custom-safe-themes (quote ("70fecb7e435889b7d0df23246b4c94343b7ed03bca0367e8e50787a8f85866d7" "007b69ffec046a5842e34fea287b23c49175dfd6c6d5a0d9cdf150a2e8a8979f" default)))
 '(ecb-options-version "2.32")
 '(show-trailing-whitespace nil))
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
;; '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
;; '(minibuffer-prompt ((t (:foreground "white")))))

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
