# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# per-terminal preferences
case x"$MYPROFILE" in
    xorg-on-gray) echo orange in force;;
    xwht-on-gray) echo white in force;;
esac

test -d /opt/csw/bin && PATH=/opt/csw/bin:$PATH
PATH=$HOME/bin:$HOME/hosts:/usr/local/bin:$PATH

test -d $HOME/perl5/bin && PATH=$HOME/perl5/bin:$PATH
export PATH

for dir in $HOME/perl /usr/local/share/perl/* /usr/local/lib/perl/* /usr/local/lib/perl5/site_perl; do
    PERL5LIB=$dir:$PERL5LIB    
done
export PERL5LIB

test -d $HOME/perl5/man && export MANPATH=$HOME/perl/man:$MANPATH
test -d /usr/local/man && export MANPATH=$MANPATH:/usr/local/man

# For MediaWiki client
export MVS_BROWSER=firefox

unset MIBS MIBDIRS

case $BASH_VERSION in
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    3*) shopt -s checkwinsize;;
esac

BC_ENV_ARGS='-l $HOME/etc/mylib.bc'

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export LESS=-inXR

# happy terminal
if [ -d $HOME/terminfo ]; then
    export TERMINFO=$HOME/terminfo
    export TERM=xterm-color
    export GREP_OPTIONS=--color=auto 
fi

YELLOW="\[\033[1;33m\]"
LTRED="\[\033[0;31m\]"
LTGRN="\[\033[0;32m\]"
LTCYN="\[\033[0;36m\]"
CLEAR="\[\033[0m\]"
CRTRS="\[\033[01;35m\]"
BLUE="\[\033[01;34m\]"

# PS1="$TITLEBAR\
# $LTGRN\u$CLEAR\
# @$LTGRN\h$CLEAR\
# :$LTGRN\w\n$YELLOW[\!] \$$CLEAR "

# root gets red prompt
if [ 0 -eq $EUID ]; then
    PS1COLOR=${LTRED}
else
    PS1COLOR=$LTGRN
fi

if type git > /dev/null 2>&1; then
    HAVEGIT=yes
    alias gsm='git status --untracked-files=no --ignore-submodules'
    alias gds='git diff --stat HEAD~1'
    source ~/.git-prompt.sh
fi

# no prompt command for console
case $TERM in
    rxvt|xterm*) 
	# 	PROMPT_COMMAND='echo -ne "${PS1COLOR}${USER}@${HOSTNAME}: ${PWD}\007"'
 	PS1="${PS1COLOR}\u@\h${CLEAR}:${BLUE}\w${YELLOW}\$(__git_ps1)${CLEAR}\$ "
 	;;

    screen|vt100)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007\033k$PWD\033\\"'
	PS1='$ ';
        ;;

    *)
	PROMPT_COMMAND=''
	PS1='\u@\h:\w\$ '
	;;
esac

#
# historystuff
#
HISTSIZE=10000
HISTTIMEFORMAT=1
MYHISTDIR=$HOME/.bash_histories
NOW=$( date '+%Y%m%d-%H%M%S' )
TTY=$( tty | sed s,/,-,g )

test -d $MYHISTDIR || mkdir $MYHISTDIR
if [ -d $MYHISTDIR ]; then  
    HISTFILE=${MYHISTDIR}/${NOW}-${HOSTNAME}${TTY}
    echo Saving history to $HISTFILE
else
    note No $MYHISTDIR directory, no history saved
fi

# http://www.webupd8.org/2010/11/alternative-to-200-lines-kernel-patch.html
#if [ "$PS1" -a $HOSTNAME == LT324011TP ] ; then  
#   mkdir -p -m 0700 /dev/cgroup/cpu/user/$$ > /dev/null 2>&1
#   echo $$ > /dev/cgroup/cpu/user/$$/tasks
#   echo "1" > /dev/cgroup/cpu/user/$$/notify_on_release
#fi

if type beep-media-player > /dev/null 2>&1; then
    AUPLAY=beep-media-player
else 
    AUPLAY='echo need to set player'
fi

export PAGER=less
export EDITOR='fe'
export VISUAL='fe'
export LANG=C

# enable color support of ls
if dircolors > /dev/null 2>&1; then
    if [ "$TERM" != "dumb" ]; then
	# http://www.linux-sxs.org/housekeeping/lscolors.html
	# black is giving trouble in some terminals so remap it
	export LS_OPTIONS='--color=auto'
	eval `dircolors -b | sed 's/;35/;33/g'`
    fi
fi

if type gzcat > /dev/null 2>&1; then
    TGZ_ZCAT=gz
elif type zcat > /dev/null 2>&1; then
    TGZ_ZCAT=no
else
    echo note: no zcat found
fi

if type oocalc > /dev/null 2>&1; then
    OFFICE=yes
fi

note () 
{
    echo ===============================================================
    echo $@
    echo ===============================================================
}

# my do it all superdeal
m()
{
    case "$1" in
	*.tar.gz|*.tgz) 
	    if [ $TGZ_ZCAT == gz ]; then
		gzcat "$1" | tar tvf - | $PAGER 
	    else
		tar ztvf "$1" | $PAGER
	    fi
	    ;;
	*.zip|*.ZIP) unzip -l "$1" | $PAGER ;;
	*.wav|*.mp3) mplayer "$1";;
	*.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp) mplayer "$1";;
	*.pnm|*.pbm|*.jpg|*.jpeg|*.JPG|*.gif|*.GIF|*.tif|*.tiff) eog "$1";;
	*.jar) jar tvf "$1" | $PAGER ;;
	*.gz)  zcat "$1" | $PAGER ;;
	*.bz2) bzcat "$1" | $PAGER ;;
	*.doc|*.DOC|*.docx) libreoffice "$1";;
        *.odp|*.ppt|*.PPT)  libreoffice "$1";;
        *.ods|*.xls|*.XLS|.xlsx|*.odt) libreoffice "$1";;
	*.djvu|*.ps|*.pdf|*.PDF) evince "$1" > /dev/null 2>&1 & ;;
	*.png|*.PNG|*.bmp|*.BMP|*.jpg|*.JPG|*.gif|*.GIF) eog "$1";;
	*) less "$1";;
    esac
}

alias l='ls $LS_OPTIONS'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias la='ls $LS_OPTIONS -lA'
alias lrt='ls -lrt'
alias mlp='m `ls -rt /tmp/*pdf|tail -1`'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias t=tail
alias acs='apt-cache search'
alias acss='apt-cache show'
alias agu='sudo apt-get update'
alias agi='sudo apt-get install'
alias bye='exit'
alias y='echo Oops\!'
alias pf='perldoc -f'
alias cf='gzip'
alias uc='gunzip'
alias r='fc -s'
alias ack='ack-grep'

alias ct='cleartool'
alias lsck='cleartool lscheckout -me -all -cview'
alias co='cleartool checkout -nc'
alias ci='cleartool checkin -nc'

# Quick column selectors.
# ps -fe | grep bash | f1 | sort -u
alias f1="awk '{print \$1}'"
alias f2="awk '{print \$2}'"
alias f3="awk '{print \$3}'"
alias f4="awk '{print \$4}'"
alias f5="awk '{print \$5}'"
alias f6="awk '{print \$6}'"
alias f7="awk '{print \$7}'"
alias f8="awk '{print \$8}'"
alias f9="awk '{print \$9}'"
alias f10="awk '{print \$10}'"
alias f11="awk '{print \$11}'"
alias f12="awk '{print \$12}'"
alias f13="awk '{print \$13}'"

#
# ..   - Does a "cd .."
# .. 3 - Does a "cd ../../.."
# 
function .. ()
{
    local arg=${1:-1};
    while [ $arg -gt 0 ]; do
	cd .. >&/dev/null;
	arg=$(($arg - 1));
    done
}
# function cd_dot_dot
# alias ..=cd_dot_dot

clean () 
{ 
    if [ $# -lt 1 ]; then
	/bin/rm -f ,* *~ .*~ \#*\#;
	echo ,* *~ .*~ \#*\#;
    else
	for i in $@;
	do
	    if [ -d "$i" ]; then
		echo ---- Cleaning $i ----;
		( builtin cd $i;
		    echo ,* *~ .*~ \#*\#;
		    /bin/rm -f ,* *~ .*~ \#*\# );
	    else
		echo Huh?;
	    fi;
	done;
    fi
}

# ps grep
p () 
{ 
    if [ $# -lt 1 ]; then
	ps faux | $PAGER
    else
    local pid=$(pgrep $1)
    if [[ -n $pid ]]; then
	ps l -p $pid
    else
	echo None found
    fi
    fi
} 

# more which
mw () 
{ 
    _xw_sub $PAGER $1 
}

# edit which
ew ()
{
    _xw_sub $EDITOR $1
}

# cat which
cw ()
{
    _xw_sub cat $1
}

_xw_sub ()
{
    local path;
    local fileres;
    local action=$1;
    shift;
    case $(builtin type -type $1) in
	alias)
	    builtin type -all $1
	    ;;
	file)
	    path=$(builtin type -path $1);
	    fileres=$(file "$path");
	    case $fileres in
		*script*)
		    $action "$path"
		    ;;
		*text*)
		    $action "$path"
		    ;;
		file)
		    $action "$path"
		    ;;
		*)
		    echo $fileres
		    ;;
	    esac
	    ;;
	*)
	    builtin type $1
	    ;;
    esac
}



# added by duckpan installer
eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)

