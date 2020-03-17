; -*- Lisp-Interaction -*-

;; home lap specific

(use-package disable-mouse
  :ensure t
  :init (global-disable-mouse-mode 1))

(use-package emacs-sqlite3
  :ensure t)

(use-package emacsql                            ; for roam
  :ensure t)

(use-package org-roam
  :after org
  :load-path "org-roam"
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
   :custom
  (org-roam-directory "~/prj/jethrokuan-braindump")
  :bind
  ("C-c n l" . org-roam)
  ("C-c n t" . org-roam-today)
  ("C-c n f" . org-roam-find-file)
  ("C-c n i" . org-roam-insert)
  ("C-c n g" . org-roam-show-graph))


(setq twitterers '(adrianco jessfraz duckduckgo ChileSpot fermatslibrary))

(setq elfeed-feeds
      (append '("http://aperiodical.com/feed/"
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
                     "https://spreadprivacy.com/rss"
                     "http://tomblomfield.com/rss"
		     "https://blog.jessfraz.com/")
	      (mapcar
	       (lambda (x) (concat "https://www.twitrss.me/twitter_user_to_rss/?user=" (symbol-name x)))
	       twitterers)))
