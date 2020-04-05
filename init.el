;; -*- Lisp-Interaction -*-

;; Minimal emacs preferences

(setq inhibit-startup-message t
      initial-scratch-message nil
      debug-on-error nil)

(defconst work-elisp "~/Dropbox/work-elisp"
  "Work-only files - we will load all .el found.")

(defconst shared-elisp "~/prj/dotfiles/shared-elisp"
  "Version controlled and visible to all boxen.")

; todo: source a refactored bash environment file
;(add-to-list 'load-path (expand-file-name "~/Elisp"))
(add-to-list 'load-path shared-elisp)
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/workbin")
(add-to-list 'exec-path "/usr/local/bin")

;; OSX Hacks
(when (memq system-type '(darwin))
  ;; open -a /Applications/Emacs.app "$@"
  ;; If you are annoyed by the fact that it opens a new frame (window) for each file -- add
  (setq ns-pop-up-frames nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)

  ;; Two metas is better for hardware reasons, this week.
  ;; see https://stackoverflow.com/questions/7743402/how-can-i-change-meta-key-from-alt-to-cmd-on-mac-in-emacs-24#7743625
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)

  ;;      ;; Env to hit docker VM. Could be evalled from bash instead.
  ;;      (setenv "DOCKER_HOST" "tcp://192.168.99.100:2376")
  ;;      (setenv "DOCKER_MACHINE_NAME" "default")
  ;;      (setenv "DOCKER_TLS_VERIFY" "1")
  ;;      (setenv "DOCKER_CERT_PATH" "/Users/Mitchell/.docker/machine/machines/default")))

  ;; Unset all docker env
  (mapcar 'setenv '("DOCKER_HOST" "DOCKER_TLS_VERIFY" "DOCKER_CERT_PATH" "DOCKER_MACHINE_NAME")))


(if (not (memq system-type '(darwin)))
  ;;; not mac
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "firefox"))

;; host specific - if a file HOSTNAME.el exists, load that
(if (condition-case nil (load-library (concat shared-elisp "/" (system-name) ".el"))
      (file-missing t))     ; ignore only this kind of error
    (message "Loaded host specific file"))

;; no point in checking if package is available, we use it too much
(require 'package)

(package-initialize)
(setq package-archives '( ; timing out ;;;
			  ("gnu" . "https://elpa.gnu.org/packages/")
                         ; ("marmalade" . "https://marmalade-repo.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")
			 ; ("melpa-stable" . "https://stable.melpa.org/packages/")
      ))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;(package-initialize)
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
	  (autoload #'search-web-dwim "search-web")
	  (defun my-search-web-dwim ()
	    "Seach words you select as region or at point."
	    (interactive nil)
	    (search-web-dwim "duck"))))


;; elfeed
;(add-to-list 'load-path (expand-file-name "~/prj/elfeed"))
;(load-library "elfeed")
;(custom-set-variables
; '(elfeed-summary-as-default t))

; elfeed-feeds moved to hostname.el

(setq elfeed-search-title-max-width 80)

(defun waste-time ()
  (interactive nil)
  (elfeed)
  (elfeed-update))

;;; see https://github.com/skeeto/elfeed/issues/190
;(add-hook 'elfeed-show-mode-hook
;	  (lambda ()
;	    (set-face-attribute 'variable-pitch (selected-frame)
;				:font (font-spec :family "Century Schoolbook" :size 12))
;	    (setq fill-column 120)
;	    (setq elfeed-show-entry-switch #'my-show-elfeed)))

(defun my-show-elfeed (buffer)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (fill-individual-paragraphs (point) (point-max))
    (setq buffer-read-only t))
  (switch-to-buffer buffer))

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml.j2" . yaml-mode))
  :ensure t)

;; remember it needs # as separators
;; see https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
	  (add-hook 'restclient-mode-hook
		    (lambda ()
		      (local-set-key (kbd "<C-enter>") 'restclient-http-send-current-stay-in-window)
		      (local-set-key (kbd "<C-return>") 'restclient-http-send-current-stay-in-window)))))

(use-package dockerfile-mode
  :ensure t)

;(use-package cider
;  :ensure t
;  :init (setq cider-lein-command "/usr/local/bin/lein"))

; (use-package elpy
;   :ensure t
;   :init (progn
; 	  (elpy-enable)
; 	  (setq python-shell-completion-native-enable nil)))

;; Kotlin and LSP
;; -----------------------------------------------------------------------------
(use-package kotlin-mode
  :ensure t)

(add-to-list 'exec-path "~/prj/kotlin-language-server/server/build/install/server/bin")

(use-package ob-kotlin
  :ensure t)

(use-package lsp-mode
  :hook (kotlin-mode . lsp)
  :commands lsp
  :bind (("M-," . lsp-find-references)
	 ("M-." . lsp-find-definition))
  :init (custom-set-variables '(lsp-kotlin-language-server-path "~/prj/kotlin-language-server/00----runme")))

;; general perf suggested for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package company-lsp :ensure t :commands company-lsp)
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language


;; -----------------------------------------------------------------------------

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
	    (define-key global-map (kbd "M-C-/") 'helm-dabbrev))
	    ;; without this, the gray+white selection bar matches other elements
  :init (set-face-attribute 'helm-selection nil
                            :background "LightYellow"
                            :foreground "black")
  :bind (("M-x" . helm-M-x)
    ;;	 ("M-." . helm-etags-select))
         ))

(use-package helm-projectile
  :ensure t
  :init (progn
          (projectile-global-mode)
          (setq projectile-completion-system 'helm)
          (helm-projectile-on))
  ;; http://tuhdo.github.io/helm-projectile.html
  :bind (("C-c p h" . helm-projectile)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file)))

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
;  :config (progn
;	    (helm-grepint-add-grep-config
;	      platform
;	      :command "ag"
;	      :arguments "--nocolor --search-zip --nogroup --java"
;	;      :extra-arguements "thingworx-common thingworx-platform-postgres thingworx-platform-common thingworx-integrationTests"
;	      :ignore-case-arg "--ignore-case"
;	      :root-directory-function helm-grepint-git-grep-locate-root)
;	    (add-to-list 'helm-grepint-grep-list 'platform))
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
;(use-package ggtags
;  :ensure t
;  :init (progn
;	  ;; this was to pass in a --scope switch, but doing it in
;	  ;; gtags.conf was a better idea.
;	  ;; (load-library "my-ggtags")
;	  (setq ggtags-global-abbreviate-filename 80 ;;;;;; "No"
;		ggtags-global-window-height 15))
;  :bind ("M-." . helm-gtags-dwim))

;;;;;;;;;;;;; alternately, ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package helm-gtags
;  :ensure t
;  :init (custom-set-variables
;	 '(helm-gtags-path-style 'relative)
;	 '(helm-gtags-ignore-case t)
;	 '(helm-gtags-auto-update t)))

;; todo need a work init and a home one BUT see host specific section, better
;; todo  (locate-dominating-file (buffer-file-name) "build.gradle")
(add-hook 'java-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil ; force indent with spaces, never TABs
                  c-basic-offset 4)
	    (set (make-local-variable 'compile-command)
		 "cd /Users/Mitchell/src/tw-server/thingworx-platform-postgres; gradle build -x test")))

;;(use-package eclim
;;  :init (my-elcim-setup)
;;  :bind ("C-c C-e c" . eclim-java-call-hierarchy))

;(defun my-eclim-setup ()
;  (message "starting my eclim setup (slow) ...")
;  (setq
;   ;; another pkg will start it for us if needed
;   ;; eclimd-executable "~/eclipse/eclimd"
;   eclim-eclipse-dirs '("~/eclipse")
;   eclim-executable  (expand-file-name "~/eclipse/eclim"))
;  (global-eclim-mode)
;  (message "... finished my eclim setup"))

;; develop eclim
;(add-to-list 'load-path (expand-file-name "~/prj/emacs-eclim/"))
;(my-eclim-setup)
;(global-set-key (kbd "C-c C-e c") 'eclim-java-call-hierarchy)

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

(defun my-display-changed-hook (disp)
  (message "Adjusting for display %s" disp)
  (cond ((equal disp '(3840 . 1080))   ; laptop + ext monitor
	 (my-set-font-size-absolute 10))
	((equal disp '(1920 . 1080))      ; just laptop
	 (my-set-font-size-absolute 12))))

(use-package dispwatch
  :ensure t
  :config (progn
	  (add-hook 'dispwatch-display-change-hooks #'my-display-changed-hook)
	  (dispwatch-mode 1)))

(use-package go-mode
;  :bind (("M-." . godef-jump)
;	 ("M-*" . pop-tag-mark))
  :config (add-hook 'before-save-hook #'gofmt-before-save t t)
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

;;(use-package magithub
;;  :after magit
;;  :config
;;  (magithub-feature-autoinject t)
;;  (setq magithub-clone-default-directory "~/github"))

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

(defconst my-org-dir          "~/org")
(defconst my-org-journal-dir  "~/org/journal")
(defconst my-inbox-orgfile    "~/org/:gtd-inbox.org")
(defconst my-projects-orgfile "~/org/:gtd-projects.org")
(defconst my-someday-orgfile  "~/org/:gtd-someday.org")
(defconst my-tickler-orgfile  "~/org/:gtd-tickler.org")

(defun my-org-todo-list ()
  (interactive nil)
  (message "Loading TODO's")
  (org-todo-list "TODO"))

(defconst my-work-sub-team '("0-me" "Jason" "John" "Justin" "Mike" "Nancy" "Nathan"))

(defun my-make-team-template ()
  (concat
   "* %U %i%?\n"
   (apply 'concat (mapcar (lambda (x) (concat "*** " x "\n\n")) my-work-sub-team))))

(setq org-capture-templates
      '(("t" "Todo [inbox]" entry
         (file+headline "~/org/:gtd-inbox.org" "Tasks")
         "* TODO %i%?")
	("u" "Work Todo [inbox]" entry
         (file+headline "~/org/:gtd-inbox.org" "Work Tasks")
         "* TODO %i%? :work:")

	;; experiment
	("l" "URL [inbox]" entry
         (file+headline "~/prj/dotfiles/shared-org/shared-inbox.org" "Incoming Links")
         "** %u %?\n%c")  ; x=clipboard

        ("s" "Standup" entry
         (file+headline "~/org/:gtd-worklog.org" "Standup")
	 (function my-make-team-template))
        ("T" "Tickler" entry
         (file+headline "~/org/:gtd-tickler.org" "Tickler")
         "* %i%? \n %U")
        ("w" "Work log" entry
         (file+headline "~/org/:gtd-worklog.org" "Work Log")
         "* %U %i\n%?")
	("p" "Problem with Polya explorations" entry (file+headline "~/org/problems.org" "Problems")
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

(use-package ob-shell)

;; ???
;(setq explicit-shell-file-name "/usr/local/bin/bash")
;(setq shell-file-name "/usr/local/bin/bash")

(setenv "PATH" (concat (expand-file-name "~/workbin:") (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "~/bin:") (getenv "PATH")))

(use-package ob-http
  :ensure t)

(use-package org-mode
  :ensure org
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c t" . org-time-stamp))	; or maybe C-c .
         ;; todo: bind  org-return-indent?
	 ;; and maybe
	 ;; (global-set-key "\C-cb" 'org-switchb)
	 ;; (global-set-key "\C-cl" 'org-store-link)

  :init (progn
	  ;; allow "structure templates" eg via "<s tab"
;	  (require 'org-tempo)

          (custom-set-variables
           '(org-babel-load-languages '((shell . t)
                                        (python . t)
                                        (perl . t)
                                        (js . t)
                                        (http . t) ; uses package ob-http
                                        (emacs-lisp . nil)))
           '(org-confirm-babel-evaluate nil))

          ;; examples
          ;;
          ;; #+BEGIN_SRC js
          ;; console.log("hello");
          ;; #+END_SRC
          ;;
          ;; #+BEGIN_SRC http :pretty
          ;; GET http://example.com
          ;; #+END_SRC


          (auto-fill-mode 1)
	  (setq
           fill-column 99
           org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
	   org-startup-indented t
	   org-startup-folded "showall"
	   org-hide-leading-stars t
	   org-agenda-files (list my-inbox-orgfile
				  my-projects-orgfile
				  my-someday-orgfile
				  my-tickler-orgfile)

	   org-refile-targets (list (cons my-projects-orgfile '(:maxlevel . 3))
				    (cons my-someday-orgfile  '(:level . 1))
				    (cons my-tickler-orgfile  '(:maxlevel . 2)))

	   ;; org-agenda-text-search-extra-files (directory-files my-org-dir t "org$")
	   org-export-with-toc nil    ;; do not generate a TOC on export please
	   org-export-with-sub-superscripts nil
	   org-directory my-org-dir
	   org-default-notes-file my-inbox-orgfile)))

(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/prj/dotfiles/shared-org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

;; (defun my-journal-find-file ()
;;   (find-file (format "~/org/00-journal/%s.org" (format-time-string "%Y-%02m-%02d")))
;;   (goto-char (point-max)))
;;					;
;; ("j" "Journal" entry (function my-journal-find-file)
;;	 "* %?\nEntered on %U\n  %i\n  %a")

(defun my-save-and-bury-buffer ()
  (interactive)
  (save-buffer)
  (bury-buffer))

;; C-c C-j  - capture
;;
(use-package org-journal
  :ensure t
;  :bind* ("C-c c" . my-save-and-bury-buffer)
  :init (setq org-journal-dir my-org-journal-dir))

(use-package deft
  :ensure t
  :bind (([f9] . my-deft))
  :init (setq deft-extension "org"
              deft-directory "~/org"
	      deft-text-mode 'org-mode))

; (use-package frame-cmds
;   :ensure t
;   :init
;   (bind-key [f11] 'toggle-max-frame))

(use-package smart-mode-line
  :ensure t
  :init (progn
	  (setq sml/no-confirm-load-theme t)
	  (setq sml/theme 'respectful)
	  (sml/setup)))

(use-package material-theme
  :ensure t)

(use-package tangotango-theme
  :ensure t)

(use-package yasnippet
  :defer t
  :ensure t
  :load-path "~/.emacs.d/snippets"
  :init (yas-global-mode 1))



; Elisp
(use-package browse-kill-ring
  :bind ("C-x 4 y" . browse-kill-ring))

;;  removes all whitespace entries from kill-ring.
;;
;;    (defun iswhite (s)
;;      (not (null (string-match "^[[:space:]]*$" s))))
;;
;;    (cl-delete-if #'iswhite kill-ring)
;;
;;  But heavy handed, would rather:
;;    (defvar browse-kill-ring-display-whitespace nil
;;      "Don't show whitespace-only entries")

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
;;(use-package saveplace
;;  :ensure t
;;  :init (progn
;;	  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
;;	  (setq-default save-place t)
;;	  (desktop-save-mode 1)))	; emacs --no-desktop ... to avoid this

;; (use-package smooth-scroll
;;   :ensure smooth-scroll
;;   :init (progn
;; 	  (smooth-scroll-mode t)
;; 	  (setq smooth-scroll/vscroll-step-size 25)))

(use-package iedit
  :ensure t)

(when window-system
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
  (global-set-key "\C-z" 'undo))

;;
;; dynamic global font handling - handles all buffers, not just current one
;; http://emacs.stackexchange.com/questions/7583/transiently-adjust-text-size-in-mode-line-and-minibuffer/7584#7584
;;
(when window-system
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

  (defun my-set-font-size-absolute (pt)
    (setq font-size-pt pt)
    (set-face-attribute 'default nil :height (* font-size-pt 10)))

  (defun modi/font-size-incr ()  (interactive) (modi/font-size-adj +1))
  (defun modi/font-size-decr ()  (interactive) (modi/font-size-adj -1))
  (defun modi/font-size-reset () (interactive) (modi/font-size-adj 0))

  (modi/font-size-reset) ; Initialize font-size-pt var to the default value

  (global-set-key (kbd "C-+") 'modi/font-size-incr)
  (global-set-key (kbd "C--") 'modi/font-size-decr)
  (global-set-key (kbd "C-)") 'modi/font-size-reset))    ; ie, shift-ctrl-0


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
  "Show deft buffer, or kill it."
  (interactive)
  (if (equal (buffer-name) "*Deft*")
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
   ;; (ggtags-mode 1)
    (setq fill-column 95
	  indent-tabs-mode nil)
    ;;; (helm-gtags-mode)			;; see also disabled gtags paragraph

    ;; I don't want this every file load, maybe in save hook though
    ;; (delete-trailing-whitespace)
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
(add-to-list 'auto-mode-alist '("\\.ts$" . js-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.xml.j2" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ini.j2" . conf-mode))

;;;; JSON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.conf.j2" . json-mode))
(add-to-list 'auto-mode-alist '("\\.conf" . json-mode))

; (use-package flymake-json		; TODO: flycheck?
;   :init (add-hook 'json-mode-hook 'flymake-json-load))

(defun json-format-and-view-region (beg end)
  "Format region as json and pop up new buffer to view it"
  (interactive "r")
  (let ((buf (get-buffer-create "*view-json*"))
	(inhibit-read-only t))
    (with-current-buffer buf
	(erase-buffer))
    (shell-command-on-region beg end "jq ." buf)
    (json-mode)
    (view-mode)
    (pop-to-buffer buf)))

(defun yank-json()
  (interactive)
  (yank)
  (json-reformat-region (point) (mark)))

(global-set-key "\C-cj" #'yank-json)


(defun my-perl-mode-hook ()
;   (load-library "mycperl")
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

;;
;; Override to skip dotted dirs
;; See https://github.com/emacs-helm/helm/issues/1668
;;
(defun helm-ff-directory-files (directory)
  "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
  (setq directory (file-name-as-directory
                   (expand-file-name directory)))
  (let* (file-error
         (ls   (condition-case err
                   (helm-list-directory directory)
                 ;; Handle file-error from here for Windows
                 ;; because predicates like `file-readable-p' and friends
                 ;; seem broken on emacs for Windows systems (always returns t).
                 ;; This should never be called on GNU/Linux/Unix
                 ;; as the error is properly intercepted in
                 ;; `helm-find-files-get-candidates' by `file-readable-p'.
                 (file-error
                  (prog1
                      (list (format "%s:%s"
                                    (car err)
                                    (mapconcat 'identity (cdr err) " ")))
                    (setq file-error t)))))
         (dot  (concat directory "."))
         (dot2 (concat directory "..")))
    (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
    ;; (append (and (not file-error) (list dot dot2)) ls)
    ;; return the files only, excluding the "." and ".."
    ls
    ))

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
    (mapcar
     'load-file
     (directory-files work-elisp t ".*\.elc?$" t)))

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
      vc-follow-symlinks t
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

(defun eshell/.. (&rest args)
  (eshell/cd (string-join
              (make-list
               (if (wholenump (car args)) (car args) 1) "..")
              "/")))

(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize "┌─[" 'face `(:foreground "green"))
	 (propertize (user-login-name) 'face `(:foreground "red"))
	 (propertize "@" 'face `(:foreground "green"))
	 (propertize (system-name) 'face `(:foreground "light blue"))
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

(if window-system (server-start))

;; I don't like fighting with customize
;; TODO - https://www.emacswiki.org/emacs/CustomizingAndSaving#toc9

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
