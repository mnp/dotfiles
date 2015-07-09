;; -*- Lisp-Interaction -*-

;; Minimal emacs preferences

(setq inhibit-startup-message t
      initial-scratch-message nil
      debug-on-error nil)

(add-to-list 'load-path (expand-file-name "~/Elisp"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path "/usr/local/bin")

;; OSX Hacks
(if (memq system-type '(darwin))
    ;;; mac
    ;; open -a /Applications/Emacs.app "$@"
    ;; If you are annoyed by the fact that it opens a new frame (window) for each file -- add
    (setq ns-pop-up-frames nil
	  browse-url-browser-function 'browse-url-default-macosx-browser)
  ;;; not mac
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "firefox"))

;; host specific
(let ((host-specific-filename 
       (concat (expand-file-name "~/Elisp/") (system-name) ".el")))
  (when (file-exists-p host-specific-filename)
      (load-file host-specific-filename)))

(load-file (expand-file-name "~/Elisp/calc.el")) 

;; no point in checking if package is available, we use it too much
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; needed by use-package :bind

(use-package hideshow)
								      ; (require 'workgroups)
;;      (use-package ace-window
;;	:bind ("C-x o" . ace-window))

(use-package compile
  :init (setq compilation-scroll-output 1	
	      compile-command "make ")
  :bind ("C-x C-k" . compile))

(use-package google-c-style)
;; TODO: rtags does references and c++ well. Note find-tag
;; advice below.

								      ;      (use-package aggressive-indent
								      ;	:init (progn
								      ;		(mapcar '(lambda (z) 
								      ;			   (add-to-list 'aggressive-indent-excluded-modes z)) 
								      ;			'(Eshell Debugger html-mode))
								      ;		(global-aggressive-indent-mode 1)))

;; helm does this 
; (use-package bs 
;   :bind ("C-x C-b" . bs-show))

(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package helm-git-grep
  :bind ("C-c g" .  helm-git-grep)
  	;; helm-git-grep-with-exclude-file-pattern
  	;; (defun helm-git-grep-get-top-dir nil "/users/Mitchell/src/tw-server/PLATFORM")
  :init (progn
	  ;; Invoke `helm-git-grep' from isearch.
	  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)

	  ;; Invoke `helm-git-grep' from other helm.
	  (eval-after-load 'helm
	    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))))

(use-package helm-match-plugin)

(use-package helm-config
  :init (progn
	  (helm-mode 1)
	  (define-key global-map [remap find-file] 'helm-find-files)
	  (define-key global-map [remap occur] 'helm-occur)
	  (define-key global-map [remap list-buffers] 'helm-buffers-list)
	  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev))
  :bind (("M-x" . helm-M-x)))
					;(unless (boundp 'completion-in-region-function)
					;  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
					;  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


(use-package duplicate-line
  :bind (("M-p" . duplicate-previous-line)
	 ("M-n" . duplicate-following-line)))

;; This binds c-.
;; we've stolen c-. (from org-time-stamp, so we need to rebind that)
(use-package dot-mode
  :init (add-hook 'find-file-hooks (lambda () (dot-mode 1)))
  :bind ("C-." . dot-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package jtags-mode
  :init (add-hook 'java-mode-hook 'jtags-mode))

;;(use-package eclim
;;  :init (my-elcim-setup)
;;  :bind ("C-c C-e c" . eclim-java-call-hierarchy))

(defun my-eclim-setup ()
  (message "starting my eclim setup (slow) ...")
  (setq 
   ;; another pkg will start it for us if needed
   ;; eclimd-executable "~/eclipse/eclimd"
   eclim-eclipse-dirs '("~/eclipse")
   eclim-executable  (expand-file-name "~/eclipse/eclim"))
  (global-eclim-mode)
  (message "... finished my eclim setup"))
   
;; develop eclim
(add-to-list 'load-path (expand-file-name "~/prj/emacs-eclim/"))
(my-eclim-setup)
(global-set-key (kbd "C-c C-e c") 'eclim-java-call-hierarchy)

(use-package gradle-mode)

(use-package ack
  ;; note: local mod in elpa/ack-1.3/ack.el, should get pushed up?
  ;;	:init (setq ack-command (executable-find "ack-grep"))
;  :init (progn
;	  (setq ack-default-directory-function 'my-ack-default-directory))
  :bind ("C-c k" . ack)
  )

(defun my-ack-default-directory (arg)
  "wrap ack-default-directory-function and reverse his behavior: if ARG is
  given, call him with none, while if no ARG is given, call him with
  4.  I want to find from project root by default."
  (ack-default-directory
   (if arg nil 4)))

(use-package extended-insert
  :bind ("C-x i" . extended-insert))

(use-package org-mode
  :bind (("C-c c" . org-capture)
	 ("C-c t" . org-time-stamp))	; or maybe C-c .
  :init (progn 
	  (setq 
	   org-todo-keywords '((sequence "TODO" "WAIT" "DONE"))
	   org-hide-leading-stars t
	   org-export-with-sub-superscripts nil
	   org-directory (expand-file-name "~/.deft")
	   org-default-notes-file (concat org-directory "/notes.org"))))

;; C-c C-j 
(use-package org-journal
  :init (setq org-journal-dir (expand-file-name "~/org/journal")))

(use-package deft
  :bind (([f9] . my-deft))
  :init (setq deft-extension "org"
	      deft-text-mode 'org-mode))

(use-package frame-cmds
  :init
  (bind-key [f11] 'toggle-max-frame))

(use-package powerline
  :init (progn 
	  (powerline-center-theme)

	  ;; Separate behavior for inactive
	  ;; buffers. smart-mode-line does this out of the box,
	  ;; switch if we get bored.
	  (set-face-attribute  'mode-line-inactive
			       nil 
			       :foreground "gray30"
			       :background "black" 
			       :box '(:line-width 1 :style released-button))))

(use-package yasnippet
  :load-path "~/.emacs.d/snippets"
  :init (yas-global-mode 1))

(use-package browsekill
  :bind ("C-x 4 y" . browse-kill-ring))

(use-package show-paren
  :init (setq 
	 show-paren-style 'expression
	 show-paren-delay 0))

(use-package flycheck)

(use-package xcscope     ;; see ~/.emacs.d/elpa/xcscope-readme.txt
  :init (cscope-setup))

(use-package find-companion-thing
  :bind ("C-x C-h" . fct/find-file))

;;
;; save cursor positions and loaded files. consider projectile et al?
;; Or https://github.com/pashinin/workgroups2
;;
(use-package saveplace
  :init (progn
	  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
	  (setq-default save-place t)
	  (desktop-save-mode 1)))	; emacs --no-desktop ... to avoid this

(if window-system 
    (progn
      ;;      (set-default-font "Mono-10")
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

(load-library "my-org-mods")
(add-hook 'org-mode-hook 'my-org-mode-hook)


(setq org-capture-templates
      '(
	("j" "Journal entry with date" plain
         (file+datetree+prompt "~/org/journal.org")
         "* "
         :unnarrowed t)

	("t" "Quick task" entry
         (file+headline "~/org/todo.org" "Tasks")
         "* TODO %^{Task}"
         :immediate-finish t)

	("T" "Full task" entry
         (file+headline "~/org/todo.org" "Tasks")
         "* TODO "
         :unnarrowed t)

	("n" "Quick note" item
	 (file+headline "~/org/notes.org" "Quick notes"))))


(defun my-deft ()
  (interactive "")
  (deft)
  (deft-refresh))

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

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

(defun my-perl-mode-hook ()
  (load-library "mycperl")
  (cperl-mode)
  (flycheck-mode)
  )

(defun my-xml-mode-hook ()
  (hs-minor-mode 1)
  (local-set-key [f11] 'sgml-expand-element)
  (local-set-key [f12] 'sgml-fold-element))

(defun my-gdb-hook ()
  (setq gdb-many-windows nil
	gdb-show-main t))

(mapcar (lambda (h) (add-hook h 'my-generic-mode-hook))
	'(emacs-lisp-mode-hook html-mode-hook php-mode-user-hook
    	  sh-mode-hook sql-mode-hook c-mode-hook c++-mode-hook
	  java-mode-hook makefile-mode-hook))

(add-hook 'perl-mode-hook 	'my-perl-mode-hook)
(add-hook 'xml-mode-hook        'my-xml-mode-hook)
(add-hook 'c-mode-hook		'my-c-mode-common-hook)
(add-hook 'c++-mode-hook	'my-c-mode-common-hook)
(add-hook 'gdb-mode-hook	'my-gdb-hook)

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
      visible-bell nil
      ring-bell-function 'ignore

      comment-column 70
      fill-column 79
      eval-expression-print-length 99
      regex-tool-backend 'perl
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
(scroll-bar-mode -1)
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

(global-set-key (kbd "M-SPC")	'my-just-one-white)
(global-set-key "\C-cr" 	'align-regexp)

;; see bs mode above, disabled
(global-set-key "\C-x\C-b" 'electric-buffer-list)


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

;(if window-system 
;    (progn
;      (if (require 'edit-server)
;	  (edit-server-start)
;	(server-start))))

(if window-system (server-start))

(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("49e5a7955b853f70d1fe751b2f896921398b273aa62f47bda961a45f80219581" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "5d9351cd410bff7119978f8e69e4315fd1339aa7b3af6d398c5ca6fac7fd53c7" default)))
 '(global-eclim-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background unspecified :foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:background unspecified :foreground "green" :slant italic :weight bold)))))
