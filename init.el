;; -*- Lisp-Interaction -*-

;; Minimal emacs preferences

(setq inhibit-startup-message t
      initial-scratch-message nil
      debug-on-error nil)

(defconst work-elisp "~/Dropbox/work-elisp" 
  "Work-only files - we will load all .el found.")

; todo: source a refactored bash environment file
;(add-to-list 'load-path (expand-file-name "~/Elisp"))
(add-to-list 'load-path (expand-file-name "~/prj/dotfiles/shared-elisp"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'exec-path "/usr/local/bin")

;; OSX Hacks
(if (memq system-type '(darwin))
    (progn
      ;; open -a /Applications/Emacs.app "$@"
      ;; If you are annoyed by the fact that it opens a new frame (window) for each file -- add
      (setq ns-pop-up-frames nil)
      (setq browse-url-browser-function 'browse-url-default-macosx-browser)

      ;; Two metas is better for hardware reasons, this week.
      ;; see https://stackoverflow.com/questions/7743402/how-can-i-change-meta-key-from-alt-to-cmd-on-mac-in-emacs-24#7743625
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'meta)
  
      ;; Env to hit docker VM. Could be evalled from bash instead.
      (setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
      (setenv "DOCKER_MACHINE_NAME" "default")
      (setenv "DOCKER_TLS_VERIFY" "1")
      (setenv "DOCKER_CERT_PATH" "/Users/Mitchell/.docker/machine/machines/default")))

(if (not (memq system-type '(darwin)))
  ;;; not mac
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "firefox"))

;; host specific
(let ((host-specific-filename
       (concat (expand-file-name "~/Elisp/") (system-name) ".el")))
  (when (file-exists-p host-specific-filename)
      (load-file host-specific-filename)))

;; no point in checking if package is available, we use it too much
(require 'package)
(setq package-archives '(; ("gnu" . "https://elpa.gnu.org/packages/")
                         ; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ; ("melpa-stable" . "https://stable.melpa.org/packages/")
      ))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :init (diminish 'abbrev-mode)
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package smart-backspace
  :ensure t
  :bind ("<backspace>" . smart-backspace))

;(setq package-enable-at-startup nil)
;(eval-when-compile
;  (require 'use-package))
; (require 'bind-key)                ;; needed by use-package :bind

(use-package hideshow
  :ensure t)
								      ; (require 'workgroups)
;;      (use-package ace-window
;;	:bind ("C-x o" . ace-window))

(use-package calc
 :init (load-library "my-calc-extras")
 :bind ("M-#" . calc))

(use-package search-web
  :ensure t
  :bind ("C-c s" . my-search-web-dwim)
  :init (progn
	  (defun my-search-web-dwim ()
	    "Seach words you select as region or at point."
	    (interactive nil)
	    (search-web-dwim "duck"))))

(use-package elfeed
  :ensure t
  ;; do this to reload feeds if you update list at runtime
  ;; (elfeed-update)
  :init (progn

	  (setq elfeed-feeds '("http://aperiodical.com/feed/"
		"http://chalkdustmagazine.com/feed/"
		"http://bit-player.org/feed"
		"http://feeds.feedburner.com/Betterexplained"
		"http://helloimnadia.com/rss"
		"https://medium.com/feed/@nayafia"
		"http://techblog.netflix.com/feeds/posts/default"
		"http://esr.ibiblio.org/?feed=rss2"
		"http://www.allthingsdistributed.com/atom.xml"
		"https://engineering.linkedin.com/blog.rss"
		"http://feeds.feedburner.com/AFewThoughtsOnCryptographicEngineering"
		"http://armstrongonsoftware.blogspot.com/feeds/posts/default"
		"http://feeds.feedburner.com/nmss/SOik"
		"http://blog.jrock.us/feeds/articles/atom"
		"http://debian-administration.org/atom.xml"
		"http://feeds.feedburner.com/FutilityCloset"
		"http://www.gabrielweinberg.com/blog/atom.xml"
		"http://feeds.feedburner.com/getrichslowly"
		"http://feeds.feedburner.com/GoogleOperatingSystem"
		"http://feeds2.feedburner.com/hackaday/LgoM"
		"http://www.cringely.com/feed/"
		"http://feeds.feedburner.com/IeeeSpectrumFullText"
		"http://lambda-the-ultimate.org/rss.xml"
		"https://emacs.wordpress.com/feed/"
		"http://www.modernperlbooks.com/mt/atom.xml"
		"http://cds-srv.sun.com:8700/rss/insert/public/sunalert_insert.xml"
		"http://www.qbyte.org/puzzles/rss2.xml"
		"http://nooface.net/nooface.rss"
		"http://www.math.columbia.edu/~woit/wordpress/?feed=rss2"
		"http://www.aaronsw.com/2002/feeds/pgessays.rss"
		"http://prog21.dadgum.com/atom.xml"
		"http://randsinrepose.com/feed/?_=5778"
		"https://www.schneier.com:443/blog/index2.rdf"
		"http://rss.sciam.com/ScientificAmerican-Global"
		"http://rss.slashdot.org/slashdot/classic"
		"http://feeds.feedburner.com/oreilly/radar/atom"
		"http://feeds.feedburner.com/tedtalks_video"
		"http://feeds2.feedburner.com/timferriss"
		"http://zenhabits.net/feed/"
		"http://feeds.feedburner.com/http/wwwslowcarbfoodiecom"
		"http://www.xkcd.com/rss.xml"
		"http://feeds.feedburner.com/JamesOnSoftware"
		"http://www.quantamagazine.org/archives/feed/"
		"http://blog.sciencevsmagic.net/feed/"
		"https://medium.com/feed/the-physics-arxiv-blog"
		"http://feeds.feedburner.com/MostlyMaths"
		"http://mindfuckmath.com/rss"
		"http://fledglingphysicist.com/feed/"
		"http://www.johndcook.com/blog/feed/"
		"http://directed-procrastination.blogspot.com/feeds/posts/default"
		"http://www.jwz.org/blog/feed/"
		"http://adamilab.blogspot.com/feeds/posts/default"
		"http://blog.tanyakhovanova.com/feed/"
		"https://firstlook.org/theintercept/feed/"
		"http://vigoroushandwaving.wordpress.com/feed/"
		"http://lwn.net/headlines/newrss"
		"http://www.preposterousuniverse.com/blog/feed/"
		"http://www.kasparov.com/feed/"
		"http://nullprogram.com/feed/"
		"http://planet.emacsen.org/atom.xml"
		"https://www.cringely.com/feed"
		"http://chalkdustmagazine.com/feed"
		"https://www.twitrss.me/twitter_user_to_rss/?user=ChileSpot"
		"https://www.twitrss.me/twitter_user_to_rss/?user=fermatslibrary"))

;;		"https://news.ycombinator.com/rss"

	  (defun waste-time ()
	    (interactive nil)
	    (elfeed)
	    (elfeed-update))

	  ;; bug? every invocation makes it larger
;	  (setq shr-width 100)
;	  (setq shr-use-fonts nil)

;	  (add-hook 'elfeed-show-mode-hook
;		    (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "DejaVuSansMono" :size 16)))

	  ))

;; remember it needs # as separators
;; see https://github.com/pashky/restclient.el
(use-package restclient
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
	  (add-hook 'restclient-mode-hook
		    (lambda ()
		      (local-set-key (kbd "<C-return>") 'restclient-http-send-current-stay-in-window)))))

(use-package dockerfile-mode
  :ensure t)

;(use-package cider
;  :ensure t
;  :init (setq cider-lein-command "/usr/local/bin/lein"))

(use-package elpy
  :ensure t
  :init (progn
	  (elpy-enable)
	  (setq python-shell-completion-native-enable nil)))

(use-package projectile
  :ensure t
  :init (projectile-mode))

(use-package compile
  :ensure t
  :init (setq compilation-scroll-output 1
	      compile-command "make ")
  :bind ("C-x C-k" . compile))

(use-package google-c-style
  :disabled t)

;; TODO: rtags does references and c++ well. Note find-tag
;; advice below.

;;      (use-package aggressive-indent
;;	:init (progn
;;		(mapcar '(lambda (z)
;;			   (add-to-list 'aggressive-indent-excluded-modes z))
;;			'(Eshell Debugger html-mode))
;;		(global-aggressive-indent-mode 1)))

;; helm does this
; (use-package bs
;   :bind ("C-x C-b" . bs-show))

(use-package git-gutter
  :ensure t
  :diminish
  :init (global-git-gutter-mode +1))

;;
;; A .dir-locals.el file helps a bunch here:
;;
;; ((nil . ((git-grep-path . "PLATFORM thingworx-platform-postgres")
;; 	 (compile-command . "cd ~/src/tw-server; ./gradlew build"))))
;;

(use-package helm
  :ensure t
  :diminish
  :config (progn
	    (helm-mode 1)
	    (helm-adaptive-mode 1)
	;    (helm-push-mark-mode 1)
	    (add-to-list 'helm-completing-read-handlers-alist
			 '(find-file . helm-completing-read-symbols))
	    (unless (boundp 'completion-in-region-function)
	      (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
	      (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
	    (define-key global-map [remap occur] 'helm-occur)
	    (define-key global-map [remap list-buffers] 'helm-buffers-list)
	    (define-key global-map (kbd "M-C-/") 'helm-dabbrev)
	    ;; without this, the gray+white selection bar matches other elements
	    (set-face-attribute 'helm-selection nil
				:background "LightYellow"
				:foreground "black"))
  :bind (("M-x" . helm-M-x)))

;; (use-package helm-git-grep
;;   :ensure helm-git-grep
;;   :bind ("C-c g" . helm-git-grep)
;;   	;; helm-git-grep-with-exclude-file-pattern
;;   	;; (defun helm-git-grep-get-top-dir nil "/users/Mitchell/src/tw-server/PLATFORM")
;;   :init (progn
;; 	  ;; Invoke `helm-git-grep' from isearch.
;; 	  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;;
;; 	  ;; Invoke `helm-git-grep' from other helm.
;; 	  (eval-after-load 'helm
;; 	    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))))

;; https://github.com/kopoli/helm-grepint
(use-package helm-grepint
  :ensure t
  :init (helm-grepint-set-default-config)
  :bind ("C-c g" . helm-grepint-grep-root)
  
  ;; (twx) can also turn this extra config by using ".dir-locals.el" in
  ;; platform projects
  :config (progn 
	    (helm-grepint-add-grep-config
	      platform
	      :command "ag"
	      :arguments "--nocolor --search-zip --nogroup --java"
	;      :extra-arguements "thingworx-common thingworx-platform-postgres thingworx-platform-common thingworx-integrationTests"
	      :ignore-case-arg "--ignore-case"
	      :root-directory-function helm-grepint-git-grep-locate-root)
	    (add-to-list 'helm-grepint-grep-list 'platform))
)

; local
(use-package duplicate-line
  :bind (("M-p" . duplicate-previous-line)
	 ("M-n" . duplicate-following-line)))

;; This binds c-.
;; we've stolen c-. (from org-time-stamp, so we need to rebind that)
(use-package dot-mode
  :ensure t
  :init (add-hook 'find-file-hooks (lambda () (dot-mode 1)))
  :bind ("C-." . dot-mode))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

; jtags mode has rotted. gnu global has many more features atm
(use-package ggtags
  :ensure t
  :init (progn
	  ;; this was to pass in a --scope switch, but doing it in
	  ;; gtags.conf was a better idea.
	  ;; (load-library "my-ggtags")
	  (setq ggtags-global-abbreviate-filename 80 ;;;;;; "No"
		ggtags-global-window-height 15)))

;;;;;;;;;;;;; alternately, ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;(use-package helm-gtags
;  :ensure t
;  :init (custom-set-variables
;	 '(helm-gtags-path-style 'relative)
;	 '(helm-gtags-ignore-case t)
;	 '(helm-gtags-auto-update t)))

;; todo need a work init and a home one
;; todo  (locate-dominating-file (buffer-file-name) "build.gradle")
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil) ; force indent with spaces, never TABs
	    (set (make-local-variable 'compile-command)
		 "cd /Users/Mitchell/src/tw-server/thingworx-platform-postgres; gradle build -x test")))

(use-package lua-mode
  :ensure t)

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

;; scala

;; depends on (executable-find "sbt")  ~~ "/usr/local/bin/sbt"
;(use-package ensime
;  :ensure t
;  ;; :pin melpa-stable
;)

;;
;; Golang
;; nice howto: http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;
(add-to-list 'exec-path "/usr/local/opt/go/libexec/bin")
(add-to-list 'exec-path "~/go/bin")

(setenv "PATH" (concat "/usr/local/bin:/usr/local/opt/go/libexec/bin:" (getenv "PATH")))
(setenv "GOPATH" (expand-file-name "~/go"))
; (setenv "GOROOT" "/usr/local/opt/go")

(use-package go-mode
;  :bind (("M-." . godef-jump)
;	 ("M-*" . pop-tag-mark))
  :init (add-hook 'before-save-hook 'gofmt-before-save)
  :ensure t)

(use-package go-eldoc
  :init (add-hook 'go-mode-hook 'go-eldoc-setup)
  :ensure t)

(use-package go-direx
  ;; reminder: might mess with popwin.el here
  :bind (:map go-mode-map
	      ("C-c ;" . go-direx-pop-to-buffer)))

;(use-package hideshow
;  :bind ((kbd "f12") . hs-toggle-hiding))


(use-package magit
  :ensure t
  :config (global-set-key "\C-xg" 'magit-status))

(use-package groovy-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode)))

(use-package gradle-mode
  :diminish
  :ensure t)

;; subsumed by grepint probably
;;
;; (use-package ack
;;   :ensure ack
;;   ;; note: local mod in elpa/ack-1.3/ack.el, should get pushed up?
;;   ;;	:init (setq ack-command (executable-find "ack-grep"))
;; ;  :init (progn
;; ;	  (setq ack-default-directory-function 'my-ack-default-directory))
;;   :bind ("C-c k" . ack)
;;   )
;;
;; (defun my-ack-default-directory (arg)
;;   "wrap ack-default-directory-function and reverse his behavior: if ARG is
;;   given, call him with none, while if no ARG is given, call him with
;;   4.  I want to find from project root by default."
;;   (ack-default-directory
;;    (if arg nil 4)))

(use-package extended-insert
  :bind ("C-x i" . extended-insert))

(defconst my-org-dir         (expand-file-name "~/org"))
(defconst my-notes-orgfile   (expand-file-name "~/org/00-notes.org"))
(defconst my-journal-dir     (expand-file-name "~/org/00-journal"))
(defconst my-task-orgfile    (expand-file-name "~/org/00-todo.org"))

(defun my-journal-find-file ()
  (find-file (format "~/org/00-journal/%s.org" (format-time-string "%Y-%02m-%02d")))
  (goto-char (point-max)))

(defun my-org-todo-list ()
  (interactive nil)
  (progn
    (message "Loading TODO's")
    (org-todo-list "TODO")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/00-todo.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (function my-journal-find-file)
	 "* %?\nEntered on %U\n  %i\n  %a")
	("p" "Problem with Polya explorations" entry (file+headline "~/org/00-problems.org" "Problems")
	 "* %U %i
** (I)dentify the problem
What is the unknown? What are the data? What is the condition?
Draw a figure. Introduce suitable notation.
*** %?

** (D)evelop a plan
Draw connection between data and the unknown. Have you seen it before? Perhaps a problem with similar unknown?
Try solving a SIMPLER problem. Try specialization and generalization of problem.
***

** (E)xecute the plan
Execute the plan that you just developed. If it doesn't work, go back to step 2 and develop another plan.
***

** (A)ssess the solution
Can you derive the solution differently? Can you use the result or method in some other problem?
***
")))

(use-package org-mode
  :ensure org
  :bind (("C-c c" . org-capture)
	 ("C-c a t" . my-org-todo-list)
         ;; todo: bind  org-return-indent
	 ("C-c t" . org-time-stamp))	; or maybe C-c .
  :init (progn
	  (setq
	   org-startup-indented t
	   org-startup-folded "showall"
	   org-todo-keywords '((sequence "TODO" "WAIT" "DONE"))
	   org-hide-leading-stars t
	   org-agenda-files '("~/org/00-todo.org" "~/org/00-notes.org" "~/org/00-journal")
	   org-export-with-sub-superscripts nil
	   org-directory (expand-file-name "~/.deft")
	   org-default-notes-file my-notes-orgfile)))

;; C-c C-j
(use-package org-journal
  :ensure t
  :init (setq org-journal-dir my-journal-dir))

(use-package deft
  :ensure t
  :bind (([f9] . my-deft))
  :init (setq deft-extension "org"
	      deft-text-mode 'org-mode))

(use-package frame-cmds
  :ensure t
  :init
  (bind-key [f11] 'toggle-max-frame))

(use-package powerline
  :ensure powerline
  :init (progn
	  ; works
	  ; (custom-set-faces
	  ; '(mode-line ((t (:foreground "Black" :background "DarkOrange" :box nil))))
	  ; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :overline "gray" :underline "gray" :box nil)))))

	  ;; see also powerline-active0, 1, 2 and -inactive0, 1, 2
	  ;; can check current values by (face-attribute 'powerline-active1 :foreground)
	  ;; TODO: (make-hud face1 face2).  Or look at Smart Mode Line
	  (set-face-attribute 'powerline-active0 nil
			      :foreground "Black"
			      :background "DarkOrange")

	  (set-face-attribute 'mode-line nil
			      :foreground "Black"
			      :background "DarkOrange")
	  (set-face-attribute 'mode-line-inactive nil
			      :foreground "#f9f9f9"
			      :background "#666666"
			      :overline "gray"
			      :underline "gray")
	  (set-face-attribute 'powerline-active1 nil 
			      :background "white"
			      :foreground "black")
	  (set-face-attribute 'mode-line-buffer-id nil
			      :overline   "gray"
			      :underline  "gray"
			      :foreground "black")
	  (powerline-default-theme)))

(use-package material-theme
  :ensure t)

; (use-package yasnippet
;   :defer t
;   :ensure t
;   :load-path "~/.emacs.d/snippets"
;   :init (yas-global-mode 1))

; Elisp
(use-package browse-kill-ring
  :bind ("C-x 4 y" . browse-kill-ring))

(use-package easy-kill
  :ensure t
  :init (progn
	  (global-set-key [remap kill-ring-save] 'easy-kill)
	  (global-set-key [remap mark-sexp] 'easy-mark)))

(use-package flycheck
  :ensure t)

(use-package xcscope     ;; see ~/.emacs.d/elpa/xcscope-readme.txt
  :ensure xcscope
  :init (cscope-setup))

; local
(use-package find-companion-thing
  :bind ("C-x C-h" . fct/find-file))

;;
;; save cursor positions and loaded files. consider projectile et al?
;; Or https://github.com/pashinin/workgroups2
;;
(use-package saveplace
  :ensure t
  :init (progn
	  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
	  (setq-default save-place t)
	  (desktop-save-mode 1)))	; emacs --no-desktop ... to avoid this

;; (use-package smooth-scroll
;;   :ensure smooth-scroll
;;   :init (progn
;; 	  (smooth-scroll-mode t)
;; 	  (setq smooth-scroll/vscroll-step-size 25)))

(use-package iedit
  :ensure t)

(if window-system
    (progn
      ;;      (set-default-font "Mono-10")
      ;;      (set-face-attribute 'default nil :font "terminus-12")

      (load-theme 'tangotango t)	; we also like zenburn, material-theme, solarized

      ;; consider all themes safe to load
      (setq custom-safe-themes t)

      ;; this maybe also good for local terminal, but how do we tell
      ;; that from a remote?
      (setq redisplay-dont-pause t)

      (mouse-avoidance-mode 'jump)
;;      (add-hook 'server-visit-hook
;;		'(lambda ()
;;		   (raise-frame)
;;		   (recenter)))
      (mouse-wheel-mode 1)
      (delete-selection-mode 1)
      (global-set-key "\C-z" 'undo)))

;;
;; dynamic global font handling - handles all buffers, not just current one
;; http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584
;;
(if window-system
    (progn
      (setq default-font-size-pt 12

	    ;; assuming many buffers persisting
            confirm-kill-emacs #'yes-or-no-p)

      (defun modi/font-size-adj (&optional arg)
	"The default C-x C-0/-/= bindings do an excellent job of font resizing.
They, though, do not change the font sizes for the text outside the buffer,
example in mode-line. Below function changes the font size in those areas too.

M-<NUM> M-x modi/font-size-adj increases font size by NUM points if NUM is +ve,
                               decreases font size by NUM points if NUM is -ve
                               resets    font size if NUM is 0."
	(interactive "p")
	(if (= arg 0)
	    (setq font-size-pt default-font-size-pt)
	  (setq font-size-pt (+ font-size-pt arg)))
	;; The internal font size value is 10x the font size in points unit.
	;; So a 10pt font size is equal to 100 in internal font size value.
	(set-face-attribute 'default nil :height (* font-size-pt 10)))

      (defun modi/font-size-incr ()  (interactive) (modi/font-size-adj +1))
      (defun modi/font-size-decr ()  (interactive) (modi/font-size-adj -1))
      (defun modi/font-size-reset () (interactive) (modi/font-size-adj 0))

      (modi/font-size-reset) ; Initialize font-size-pt var to the default value

      (global-set-key (kbd "C-+") 'modi/font-size-incr)
      (global-set-key (kbd "C--") 'modi/font-size-decr)
      (global-set-key (kbd "C-)") 'modi/font-size-reset)    ; ie, shift-ctrl-0
))


;; ------------------------------------------------------
;; Modes
;; ------------------------------------------------------

(set-face-attribute 'show-paren-match nil
		    :background "DarkOrange4"
		    :foreground "white")
(setq
 show-paren-style 'expression
 show-paren-delay 0)

;; (require 'erc)
;;
;; (erc-autojoin-mode t)
;; (setq erc-autojoin-channels-alist
;;       '((".*\\.perlhack.net" "#gaia")))
;; (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
;;
;; (defun my-erc-start-or-switch ()
;;   "Connect to ERC, or switch to last active buffer"
;;   (interactive)
;;   (if (get-buffer "#gaia") ;; ERC already active?
;;       (erc-track-switch-buffer 1) ;; yes: switch to last active
;;     (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
;;       (erc :server "irc.perlhack.net" :port 6667 :nick "mnp" :full-name "mitchell perilstein"))))
;;
;; (global-set-key (kbd "C-c e") 'my-erc-start-or-switch)

(load-library "my-org-mods")
(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-deft ()
  (interactive "")
  (if (equalp (buffer-name) "*Deft*")
      (kill-buffer deft-buffer)
    (deft)))

;(defun my-find-file-hook ()
;  ;; not good idea along with sshct, which see
;;  (git-gutter)
;    ;;  (vc-mode-line (buffer-file-name) 'git)
;  )
;(add-hook 'find-file-hooks 'my-find-file-hook)

(defun my-prog-mode-hook ()
  (show-paren-mode 1)
    (ggtags-mode 1)
  ;;; (helm-gtags-mode)			;; see also disabled gtags paragraph
)

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(defun my-generic-mode-hook ()
  (auto-fill-mode 1)
;  (hs-minor-mode 1)
  )

(defun my-c-mode-common-hook ()
  (google-set-c-style)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil) ; force indent with spaces, never TABs
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

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

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

(add-to-list 'auto-mode-alist '("\\.xml.j2" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ini.j2" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.yml.j2" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.conf.j2" . json-mode))
(add-to-list 'auto-mode-alist '("\\.conf" . json-mode))

(defun my-perl-mode-hook ()
  (load-library "mycperl")
  (cperl-mode)
  (flycheck-mode)
  )

(defun my-xml-mode-hook ()
  (hs-minor-mode 1)
  (set-variable nxml-child-indent 4)
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

(defun get-shell-file-env (FILE VAR)
  "Use bash to source FILE in a temporary subshell and report the value of env VAR."
  (let ((F (expand-file-name FILE)))
    (if (file-readable-p F)
	(shell-command-to-string
	 (format "bash -c '. %s; echo -n ${%s}' 2>/dev/null" F VAR))
      nil)))

;; whooops see also .dir-locals.el - already invented

(defun work()
  "switch to main work context"
  (interactive nil)
  (let ((workdir (get-shell-file-env "~/.work" "WORK")))
    (message workdir)
    (find-file workdir)))

(defun my-helm-grepint-git-grep-locate-root ()
  (expand-file-name
  (or (locate-dominating-file (file-name-as-directory
			       (expand-file-name (file-truename default-directory)))
			      ".mnp-project")
      (locate-dominating-file (file-name-as-directory
			       (expand-file-name (file-truename default-directory)))
			      ".git"))))

;(defun get-restricted-git-command ()
;  (concat "--no-pager grep --line-number --no-color -- "
;	  (get-shell-file-env
;	   (concat (my-helm-grepint-git-grep-locate-root) "/.mnp-project")
;	   "git_grep_path")))

; (defvar my-hs-hide nil "Current state of hideshow for toggling all.")

;(defun my-toggle-hideshow-all ()
;  "Toggle hideshow all."
;  (interactive)
;  (setq my-hs-hide (not my-hs-hide))
;  (if my-hs-hide
;      (progn (hs-hide-all) (message "hid"))
;    (progn (hs-show-all) (message "show"))))

;(defun my-toggle-selective-display (column)
;  "cheap hideshow - http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/"
;  (interactive "P")
;  (set-selective-display
;   (or column ; disable toggle if column was supplied
;       (unless selective-display 1))))

    ;; https://www.emacswiki.org/emacs/HideShow

    (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

    ;(defun toggle-hiding (column)
    (defun my-toggle-selective-display (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))


(ffap-bindings) ;; replaces find-file

;; must come after ffap is set up
(if (file-exists-p work-elisp)
    (mapcar 'load-file work-elisp))

;;; ggtags does this based on project root already
;(defadvice find-tag (before find-tags-table () activate)
;  "find-tag (M-.) will load ./TAGS by default, the first time you use
;it.  This will look in parent dirs up to root for it as well."
;  (or (get-buffer "TAGS")
;      (let ((tagfile (concat (locate-dominating-file (buffer-file-name) "TAGS") "TAGS")))
;	(if tagfile
;	    (visit-tags-table tagfile)
;	  (error "Can't find TAGS looking upwards from %s" default-directory)))))

;; idea: compile() can look upwards until it finds a makefile

;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

; experimental
(defadvice yes-or-no-p (after acknowdge activate)
  "User should get immediate feedback from a y-or-n response. Package, for example, is bad at this."
  (message "working...")
  (sleep-for 1))

;; edit-which
(defun ew (prog)
  (interactive "sProgram: ")
  (find-file (or (executable-find prog)
		 (error (concat prog " not found in exec-path")))))

;; more-which
(defun mw (prog)
  (interactive "sProgram: ")
  (view-file (or (executable-find prog)
		 (error (concat prog " not found in exec-path")))))

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

(defun my-format-xml-buffer ()
  "Format entire XML buffer."
  (interactive "")
  (shell-command-on-region (point-min) (point-max) "xmllint --format -" t t))

(defun pop-to-scratch ()
  "If in *scratch*, bury it, otherwise pop to it."
  (interactive nil)
  (if (equal (buffer-name) "*scratch*")
      (bury-buffer)
    (switch-to-buffer "*scratch*")))

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
      indent-tabs-mode nil
      comment-column 80
      fill-column 95
      eval-expression-print-length 99
      regex-tool-backend 'perl
      c-style-variables-are-local-p nil

      ;; i hate splitting vertically in most cases
      split-width-threshold nil

      ;; disable novice mode
      disabled-command-function nil
)

(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize "┌─[" 'face `(:foreground "green"))
	 (propertize (user-login-name) 'face `(:foreground "red"))
	 (propertize "@" 'face `(:foreground "green"))
	 (propertize (system-name) 'face `(:foreground "blue"))
	 (propertize "]──[" 'face `(:foreground "green"))
	 (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
	 (propertize "]──[" 'face `(:foreground "green"))
	 (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
	 (propertize "]\n" 'face `(:foreground "green"))
	 (propertize "└─>" 'face `(:foreground "green"))
	 (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))
	 )))

;; save minibuffer history across sessions
(savehist-mode 1)

;; see discussion http://www.reddit.com/r/emacs/comments/10rdl2/globalautorevertmode_1/
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; if we don't use vc saves go much faster if nil?
;(setq vc-handled-backends '(Git))
(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

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
(global-set-key [f3]            'pop-to-scratch)
(global-set-key [f4]            'my-code-search)
(global-set-key [f5]   		(lambda () (interactive) (revert-buffer t nil)))
(global-set-key [f6]   		(lambda () (interactive) (switch-to-buffer nil)))
(global-set-key [f7]   		'my-toggle-hideshow-all)
(global-set-key [f12]  		'my-toggle-selective-display)
(global-set-key [home] 		'beginning-of-buffer)
(global-set-key [end] 		'end-of-buffer)

(global-set-key "\M-g" 		'goto-line)
(global-set-key [?\C-_] 	'help-command)
(global-set-key "\C-h"	 	'backward-delete-char)

(global-set-key (kbd "M-SPC")	'my-just-one-white)
(global-set-key "\C-cr" 	'align-regexp)

;; see bs mode above, disabled
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; experiment

;; minimap.  see also sublimity, with different bugs
;(use-package minimap
;  :init
;  (progn
;    (setq minimap-major-modes '(prog-mode text-mode)
;	  minimap-window-location 'right)
;    (defface minimap-active-region-background
;      '((((background dark)) (:background "#660000"))
;	(t (:background "#C847D8FEFFFF")))
;      "Face for the active region in the minimap.
;By default, this is only a different background color."
;  :group 'minimap)))


;;
;; Sync elpa package list on all machines.  Save to a file and version
;; control it.
;;
(defvar my-shared-package-file (concat user-emacs-directory "all-platforms-packages.el"))

(defun my-elpa-package-names ()
  "List all the elpa packages present"
  (sort (mapcar 'car package-alist) 'string-lessp))

(defun my-elpa-save-shared-packages ()
  (if (file-exists-p my-shared-package-file)
      (rename-file my-shared-package-file (concat my-shared-package-file ".old") t))
  (save-excursion
    (set-buffer (find-file-noselect my-shared-package-file))
    (print ";; see my-elpa-save-shared-packages()")
    (print (my-elpa-package-names))
    (basic-save-buffer))
  nil)

;    (my-elpa-save-shared-packages)
; (package-install-from-archive pkg-desc)

(defun find-file-most-recent (dir)
  "Open most recently created file in DIR."
  (let ((files (directory-files-and-attributes dir nil nil t)))
    (find-file (concat dir "/" (caadr (sort
				       files
				       (lambda (a b) (time-less-p (nth 6 b) (nth 6 a)))))))))

(defun erd ()
  "Edit most Recent Download"
  (interactive nil)
  (find-file-most-recent "~/Downloads"))

(defun mrd ()
  "View most Recent Download"
  (interactive nil)
  (erd)
  (log-view-mode))

(defalias 'adventure 'dunnet)

;; keep this last

;(if window-system
;    (progn
;      (if (require 'edit-server)
;	  (edit-server-start)
;	(server-start))))


(if window-system (server-start))

;; I don't like fighting with customize
;; TODO - https://www.emacswiki.org/emacs/CustomizingAndSaving#toc9

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8790269696322ff6821d75414c7d6ea8726d204cdeadedfd04c87b0c915296f7" "4e63466756c7dbd78b49ce86f5f0954b92bf70b30c01c494b37c586639fa3f6f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(elfeed-feeds
   (quote
    ("http://aperiodical.com/feed/" "http://chalkdustmagazine.com/feed/" "http://bit-player.org/feed" "http://feeds.feedburner.com/Betterexplained" "http://helloimnadia.com/rss" "https://medium.com/feed/@nayafia" "http://techblog.netflix.com/feeds/posts/default" "http://esr.ibiblio.org/?feed=rss2" "http://www.allthingsdistributed.com/atom.xml" "https://engineering.linkedin.com/blog.rss" "http://feeds.feedburner.com/AFewThoughtsOnCryptographicEngineering" "http://armstrongonsoftware.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/nmss/SOik" "http://blog.jrock.us/feeds/articles/atom" "http://debian-administration.org/atom.xml" "http://feeds.feedburner.com/FutilityCloset" "http://www.gabrielweinberg.com/blog/atom.xml" "http://feeds.feedburner.com/getrichslowly" "http://feeds.feedburner.com/GoogleOperatingSystem" "http://feeds2.feedburner.com/hackaday/LgoM" "http://www.cringely.com/feed/" "http://feeds.feedburner.com/IeeeSpectrumFullText" "http://lambda-the-ultimate.org/rss.xml" "https://emacs.wordpress.com/feed/" "http://www.modernperlbooks.com/mt/atom.xml" "http://cds-srv.sun.com:8700/rss/insert/public/sunalert_insert.xml" "http://www.qbyte.org/puzzles/rss2.xml" "http://nooface.net/nooface.rss" "http://www.math.columbia.edu/~woit/wordpress/?feed=rss2" "http://www.aaronsw.com/2002/feeds/pgessays.rss" "http://prog21.dadgum.com/atom.xml" "http://randsinrepose.com/feed/?_=5778" "https://www.schneier.com:443/blog/index2.rdf" "http://rss.sciam.com/ScientificAmerican-Global" "http://rss.slashdot.org/slashdot/classic" "http://feeds.feedburner.com/oreilly/radar/atom" "http://feeds.feedburner.com/tedtalks_video" "http://feeds2.feedburner.com/timferriss" "http://zenhabits.net/feed/" "http://feeds.feedburner.com/http/wwwslowcarbfoodiecom" "http://www.xkcd.com/rss.xml" "http://feeds.feedburner.com/JamesOnSoftware" "http://www.quantamagazine.org/archives/feed/" "http://blog.sciencevsmagic.net/feed/" "https://medium.com/feed/the-physics-arxiv-blog" "http://feeds.feedburner.com/MostlyMaths" "http://mindfuckmath.com/rss" "http://fledglingphysicist.com/feed/" "http://www.johndcook.com/blog/feed/" "http://directed-procrastination.blogspot.com/feeds/posts/default" "http://www.jwz.org/blog/feed/" "http://adamilab.blogspot.com/feeds/posts/default" "http://blog.tanyakhovanova.com/feed/" "https://firstlook.org/theintercept/feed/" "http://vigoroushandwaving.wordpress.com/feed/" "http://lwn.net/headlines/newrss" "http://www.preposterousuniverse.com/blog/feed/" "http://www.kasparov.com/feed/" "http://nullprogram.com/feed/" "https://news.ycombinator.com/rss" "http://planet.emacsen.org/atom.xml" "https://www.cringely.com/feed" "http://chalkdustmagazine.com/feed")))
 '(gradle-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (material-theme search-web srcery-theme elfeed-web diminish lua-mode solarized-theme zenburn-theme helm-gtags "sokoban" lognav-mode ag helm-ag 2048-game cakecrumbs exec-path-from-shell elfeed smart-backspace toml-mode elpy helm-projectile projectile yahtzee json-navigator hierarchy erlang go-direx tree-mode json-mode dockerfile-mode go-eldoc hackernews helm-google iedit groovy-mode helm-grepint easy-kill go-mode ggtags git-gutter restclient-helm restclient browse-kill-ring yaml-mode svg deft gradle-mode yasnippet yari xcscope use-package tangotango-theme sx svg-mode-line-themes svg-clock slime-volleyball powerline paredit org-journal magit helm-git-grep google-c-style git-gutter-fringe git-gutter+ frame-cmds flycheck emacs-eclim dot-mode company auto-complete aggressive-indent ack ace-window)))
 '(safe-local-variable-values
   (quote
    ((git-grep-path . "thingworx-platform-common thingworx-platform-postgres")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background unspecified :foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:background unspecified :foreground "green" :slant italic :weight bold))))
 '(mode-line ((t (:foreground "Black" :background "DarkOrange" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
