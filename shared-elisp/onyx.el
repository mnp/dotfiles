; -*- Lisp-Interaction -*-

;; lap specific

(use-package disable-mouse
  :ensure t
  :init (global-disable-mouse-mode 1))

(use-package org-roam
  :after org
  :load-path "org-roam"
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
   :custom
  (org-roam-directory "~/Dropbox/org-roam")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))
