
 ; todo: source a refactored bash environment file
+;(require 's)
+;(defun load-bash-env ()
+;  (let ((bashpath (s-chomp (shell-command-to-string "bash -c 'set -a; . ~/.bashrc; echo $PATH'"))))
+;    (setenv "PATH" bashpath)
+;    (mapcar
+;     (lambda (d) (add-to-list 'exec-path d))
+;     (s-split ":" bashpath))))
+
+;(load-bash-env)
