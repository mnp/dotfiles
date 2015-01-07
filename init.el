;; Minimal emacs preferences

(setq inhibit-startup-message t
      initial-scratch-message nil
      debug-on-error nil)

(add-to-list 'load-path (expand-file-name "~/Elisp"))

(if (condition-case nil 
	(require 'package)
      (error nil))
    (progn
      
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.org/packages/") t)
      (package-initialize)
      (setq package-enable-at-startup nil)

      ;; see describe-personal-keybindings for some of his effects
      (require 'use-package)

      (require 'hideshow)
					;(require 'dot-mode)
					; (require 'workgroups)
      (require 'google-c-style)
      ;; TODO: rtags does references and c++ well. Note find-tag
      ;; advice below.

      (require 'git-gutter)

      (use-package aggressive-indent
	:init (progn
		(mapcar '(lambda (z) 
			   (add-to-list 'aggressive-indent-excluded-modes z)) 
			'(Eshell Debugger html-mode))
		(global-aggressive-indent-mode 1)))

      (use-package bs
	:bind ("C-x C-b" . bs-show))

      (use-package duplicate-line
	:bind (("M-p" . duplicate-previous-line)
	       ("M-n" . duplicate-following-line)))

      (use-package wikipedia-mode
	:mode "\\.wiki\\'")

      (use-package markdown-mode
	:mode "\\.md\\'")

      (use-package extended-insert
	:bind ("C-x i" . extended-insert))

      (use-package deft
	:bind (([f9] . deft))
	:init (setq deft-extension "org"
		    deft-text-mode 'org-mode))

      (use-package frame-cmds
	:init
	(bind-key [f11] 'toggle-max-frame))
      
      (use-package powerline
	:init (powerline-default-theme))

      (use-package yasnippet
	:load-path "~/.emacs.d/snippets"
	:init (yas-global-mode 1))

      (use-package browsekill
	:bind ("C-x 4 y" . browse-kill-ring))

      (use-package flycheck)

      (use-package xcscope     ;; see ~/.emacs.d/elpa/xcscope-readme.txt
	:init (cscope-setup))

      (use-package find-companion-thing
	:bind ("C-x C-h" . fct/find-file))
      ))

  (if window-system 
      (progn
	
					;      (set-default-font "Mono-10")
	;;      (set-face-attribute 'default nil :font "terminus-12")

	(load-theme 'tangotango t)

	;; busted
	(global-set-key (kbd "C-+") 'text-scale-increase)
	(global-set-key (kbd "C--") 'text-scale-decrease)

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

;; org mode
					;(autoload 'org-mode "org")

;; The following lines are always needed.  Choose your own keys.
					;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
					;(define-key global-map "\C-cl" 'org-store-link)
					;(define-key global-map "\C-ca" 'org-agenda)

(setq org-todo-keywords 
      '((sequence "TODO" "WAIT" "DONE")))

(load-library "my-org-mods")
(setq org-hide-leading-stars t)
(setq org-export-with-sub-superscripts nil)
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(defun my-find-file-hook ()
  ;; not good idea along with sshct, which see
    ;;  (git-gutter)
    ;;  (vc-mode-line (buffer-file-name) 'git)
  )

(add-hook 'find-file-hooks 'my-find-file-hook)

(defun my-generic-mode-hook ()
  (auto-fill-mode 1)
;  (hs-minor-mode 1)
  )

(defun my-c-mode-common-hook ()
  (google-set-c-style)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil) ; force indent with spaces, never TABs
  (setq fill-column 79)
  (git-gutter)
  (flycheck-mode)
)

(defun indent-buffer ()
  "Re-indent the whole buffer, untabify, remove trailing whitespace."
  (interactive)
  (let ((c-indentation-style "stroustrup"))
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(add-to-list 'completion-ignored-extensions ".dep")

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

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

(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))

(defun my-perl-mode-hook ()
  (load-library "mycperl")
  (cperl-mode)
  (flycheck-mode)
  )

(defun my-xml-mode-hook ()
  (hs-minor-mode 1)
  (local-set-key [f11] 'sgml-expand-element)
  (local-set-key [f12] 'sgml-fold-element))

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


(ffap-bindings) ;; replaces find-file

(defadvice find-tag (before find-tags-table () activate)
  "find-tag (M-.) will load ./TAGS by default, the first time you use
it.  This will look in parent dirs up to root for it as well."
  (or (get-buffer "TAGS")
      (let ((tagfile (concat (locate-dominating-file (buffer-file-name) "TAGS") "TAGS")))
	(if tagfile
	    (visit-tags-table tagfile)
	  (error "Can't find TAGS looking upwards from %s" default-directory)))))

;; idea: compile() can look upwards until it finds a makefile

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
  "Make, or switch to, a perldb or gud buffer. Switch back if we're already there."
  (interactive)
  (cond ((and (boundp 'gud-comint-buffer) (buffer-live-p gud-comint-buffer))
	 (if (eq (current-buffer) gud-comint-buffer)
	     (switch-to-buffer nil) 		 ; there already - switch back to previous
	   (switch-to-buffer gud-comint-buffer)) ; not there, so go there
	 ;; gdb specific
	 (if (equal gud-minor-mode 'gdbmi)
	     (gdb-restore-windows)))
	(t (call-interactively 'perldb))))  ; not running, start it and go there

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
      compile-command "make "
      c-style-variables-are-local-p nil

      ;; i hate splitting vertically in most cases
      split-width-threshold nil
)

;; save minibuffer history across sessions
(savehist-mode 1)

;; see discussion http://www.reddit.com/r/emacs/comments/10rdl2/globalautorevertmode_1/
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; if we don't use vc saves go much faster if nil? 
;(setq vc-handled-backends '(Git))
(setq vc-handled-backends nil)

(auto-fill-mode 1)
(auto-compression-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(show-paren-mode t)

;; hilight current line - kinda annoying
;
; (global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

;; shift-arrows in addition to "C-x o"
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key [f1] 		'switch-to-most-recent-org-buffer)
(global-set-key [f2] 		'narrow-or-widen-dwim)
(global-set-key [f3]            'my-perldb)
(global-set-key [f4]            'my-code-search)
(global-set-key [f5]   		(lambda () (interactive) (revert-buffer t nil)))
(global-set-key [f6]   		(lambda () (interactive) (switch-to-buffer nil)))
(global-set-key [f7]   		'my-toggle-hideshow-all)
(global-set-key [f12]  		'my-toggle-selective-display)
(global-set-key [home] 		'beginning-of-buffer)
(global-set-key [end] 		'end-of-buffer)

(global-set-key "\M-#" 		'calc)
(global-set-key "\M-g" 		'goto-line)
(global-set-key [?\C-_] 	'help-command)
(global-set-key "\C-h"	 	'backward-delete-char)

(global-set-key "\C-x\C-k" 	'compile)
(global-set-key "\M-SPC" 	'my-just-one-white)
(global-set-key "\C-cr" 	'align-regexp)

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

(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("5d9351cd410bff7119978f8e69e4315fd1339aa7b3af6d398c5ca6fac7fd53c7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
