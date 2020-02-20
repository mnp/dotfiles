; -*- Lisp-Interaction -*-
;
; work
;

(use-package graphql-mode
  :ensure t)

(use-package ob-graphql
  :ensure t)

(add-to-list 'exec-path "/Users/mperilstein/.jenv/versions/1.8/bin")


(setq elfeed-feeds '("https://stackoverflow.com/feeds/tag/amazon-qldb"
                     "https://forums.aws.amazon.com/rss/rssmessages.jspa?forumID=353"
                     "https://github.com/fwcd/kotlin-language-server/commits/master.atom"
                     "https://github.com/amzn/ion-java/commits/master.atom"
                     "https://github.com/partiql/partiql-lang-kotlin/commits/master.atom"))

;; twitrss.me having API troubles with twitter
;; "https://www.twitrss.me/twitter_user_to_rss/?user=fermatslibrary"
