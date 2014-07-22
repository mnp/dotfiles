set print pretty
set print elements 500
define z
  print * $arg0
end
set confirm no
set history save
handle SIGPIPE nostop
handle SIGHUP nostop
handle SIGUSR2 nostop
