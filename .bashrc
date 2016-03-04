
# Noninteractive section

. ~/lib/shlib.bash


for d in /usr/man /usr/share/man /usr/local/man $HOME/perl5/man; do 
    pathadd MANPATH $d
done
export MANPATH

case $OSTYPE in
    linux*)  OS=linux;;
    darwin*) OS=darwin;;
esac

PATH=$HOME/bin:$HOME/hosts:/usr/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin
pathadd PATH /usr/local/go/bin
pathadd PATH $HOME/perl5/bin
pathadd PATH $HOME/osbin    
pathadd PATH $HOME/workbin  
pathadd PATH $HOME/homebin  
export PATH

for dir in $HOME/perl $HOME/perl5/lib/perl5 /usr/local/share/perl/* /usr/local/lib/perl/* /usr/local/lib/perl5/site_perl; do
    pathadd PERL5LIB $dir
done
export PERL5LIB

# For MediaWiki client
export MVS_BROWSER=firefox

# Go lang
pathadd PATH /usr/local/go/bin

if [ -d $HOME/go ]; then
    GOPATH=$HOME/go
    pathadd PATH $GOPATH/bin
fi
export GOPATH

unset MIBS MIBDIRS

# Interactive section
# If not running interactively, don't do anything

[ -z "$PS1" ] && return

case $BASH_VERSION in
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    3*) shopt -s checkwinsize;;
esac

# various formatters use these
export COLUMNS LINES

export BC_ENV_ARGS='-l $HOME/etc/mylib.bc'

# OSX
if type brew > /dev/null 2>&1; then
    # iterm
    nametab() { echo -ne "\033]0;"$@"\007"; }
    cd()      { builtin cd $1; nametab `basename $PWD`; }
fi

# this load all the .d files
BASH_COMPLETION=${BASH_COMPLETION:-/usr/local/etc/bash_completion}
test -f $BASH_COMPLETION && . $BASH_COMPLETION

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
WHITE="\[\033[01;37m\]"
MAGEN="\[\033[01;35m\]"

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
    alias gsm=' git status --untracked-files=no --ignore-submodules'
    alias gds=' git diff --stat'
    alias gdp=' git diff        HEAD~1 --'
    alias gdps='git diff --stat HEAD~1 --'
    alias gls='git ls-files -t'
#    alias gg='git grep'

    function gg() {
	local top=$(git rev-parse --show-toplevel)
	if [ -f $top/.mnp-project ]; then
	    . $top/.mnp-project
	else 
	    git_grep_path=.
	fi
	cd $top
	git grep $1 -- $git_grep_path
    }
    if [ -f ~/.git-prompt.sh ]; then
	source ~/.git-prompt.sh
    fi

    if [ -f ~/.git-completion.bash ]; then
	source ~/.git-completion.bash
    fi
fi

# disable ctrl-s software flow control
stty -ixon

# no prompt command for console
case $TERM in
    rxvt|xterm*) 
 	PS1BASE="${PS1COLOR}\u@\h${CLEAR}:${WHITE}\w${YELLOW}\$(__git_ps1)${CLEAR}\$ "
 	;;

    screen|vt100)
	PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}:${PWD}\007\033k$PWD\033\\"'
	PS1BASE='$ ';
	;;

    *)
	PROMPT_COMMAND=''
	PS1BASE='\u@\h:\w\$ '
	;;
esac

PS1="$PS1BASE"

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
else
    export LS_OPTIONS='-G'
fi

if type gzcat > /dev/null 2>&1; then
    ZCAT=gzcat
elif type zcat > /dev/null 2>&1; then
    ZCAT=no
else
    echo note: no zcat found
fi

if type oocalc > /dev/null 2>&1; then
    OFFICE=yes
fi

# my do it all superdeal - consider customized mailcap type alternative
m()
{
    if [ -d "$1" ]; then
	echo "-- $1 is a directory --"
	ls $LS_OPTIONS -l "$1"
	return
    fi

    # text cases
    case "$1" in
	*.tar.gz|*.tgz) 
	    if [ $ZCAT != no ]; then
		gzcat "$1" | tar tvf - | $PAGER 
	    else
		tar ztvf "$1" | $PAGER
	    fi
	    return
	    ;;
	*.jar) jar tvf "$1" | $PAGER ; return;;
	*.gz)  $ZCAT "$1" | $PAGER ; return;;
	*.bz2) bzcat "$1" | $PAGER ; return;;
	*.zip|*.ZIP) unzip -l "$1" | $PAGER ; return;;
	*.pod) perldoc "$1" | $PAGER ; return;;
	*.[123456789n]|*.3pm) nroff -man "$1" | $PAGER; return;;
    esac

    if [ -d /Applications ]; then
	case "$1" in
	    *.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp|*.wav|*.mp3|\
	    *.pnm|*.pbm|*.jpg|*.jpeg|*.JPG|*.gif|*.GIF|*.tif|*.tiff|\
	    *.doc|*.DOC|*.docx|\
	    *.odp|*.ppt|*.pptx|*.PPT|\
	    *.ods|*.xls|*.XLS|*.xlsx|*.odt|\
	    *.djvu|*.ps|*.pdf|*.PDF|\
	    *.png|*.PNG|*.bmp|*.BMP|*.jpg|*.JPG|*.gif|*.GIF|*.html) open "$1";;
	    *) less "$1";;
	esac
    else
	case "$1" in
	    *.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp) mplayer "$1";;
	    *.wav|*.mp3) vlc "$1";;
	    *.pnm|*.pbm|*.jpg|*.jpeg|*.JPG|*.gif|*.GIF|*.tif|*.tiff) eog "$1";;
	    *.doc|*.DOC|*.docx) libreoffice "$1";;
	    *.odp|*.ppt|*.PPT)  libreoffice "$1";;
	    *.ods|*.xls|*.XLS|.xlsx|*.odt) libreoffice "$1";;
	    *.djvu|*.ps|*.pdf|*.PDF) evince "$1" > /dev/null 2>&1 & ;;
	    *.png|*.PNG|*.bmp|*.BMP|*.jpg|*.JPG|*.gif|*.GIF) eog "$1";;
	    *) less "$1";;
	esac
    fi
}

# more the last "ls -rt"
mrt () 
{ 
    test -z $1 && set .
    $PAGER $1/$(ls -rt $1|tail -1)
}

alias l='ls $LS_OPTIONS'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias la='ls $LS_OPTIONS -lA'
alias lrt='ls -lrt'
alias lh='ls -lhS'
alias mlp='m `ls -rt /tmp/*pdf|tail -1`'
alias rm='rm -i'
alias Rm='command rm -f'
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
alias r='fc -s'
alias ig='grep -i'

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

type ack    > /dev/null 2>&1 || alias ack='ack-grep'
type gradle > /dev/null 2>&1 || alias gd='gradle'

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
	local pid=$(pgrep -f $1)
	if [[ -n $pid ]]; then
	    ps lww -p $pid
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


#
# select java versions; provide java7 and java8 aliases to switch on
# the fly
#

if [ -x /usr/libexec/java_home ]; then
    # osx-ish
    export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
    export JAVA_7_HOME=$(/usr/libexec/java_home -v1.7)

    rejava()
    {
	export JAVA_HOME="$1"
	echo JAVA_HOME is now "$1"

	# normalize paths and then append
 	[[ $PATH    =~ (.*):$JAVA_7_HOME/bin:(.*) ]] &&    PATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
 	[[ $MANPATH =~ (.*):$JAVA_7_HOME/man:(.*) ]] && MANPATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
 	[[ $PATH    =~ (.*):$JAVA_8_HOME/bin:(.*) ]] &&    PATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
 	[[ $MANPATH =~ (.*):$JAVA_8_HOME/man:(.*) ]] && MANPATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
	export PATH=$PATH:$JAVA_HOME/bin
	export MANPATH=$MANPATH:$JAVA_HOME/man
    }

    java7() 
    {
	rejava $JAVA_7_HOME
	PS1=${MAGEN}7${CLEAR}:$PS1BASE
    }

    java8()
    {
	rejava $JAVA_8_HOME
	PS1=${MAGEN}8${CLEAR}:$PS1BASE
    }

elif [ -x /usr/sbin/update-java-alternatives ]; then

    # linux-ish /usr/sbin/update-java-alternatives
    java7() 
    {
       sudo update-java-alternatives -s java-7-oracle
       PS1=${MAGEN}7${CLEAR}:$PS1BASE
    }

    java8()
    {
       sudo update-java-alternatives -s java-8-oracle
       PS1=${MAGEN}8${CLEAR}:$PS1BASE
    }

fi

# default
java8


if [ -d ${HOME}/perl5/lib/perl5/local/lib.pm ]; then
    # added by duckpan installer
    eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)
fi					    
					     

PERL_MB_OPT="--install_base \"/Users/Mitchell/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/Mitchell/perl5"; export PERL_MM_OPT;

type docker-machine > /dev/null 2>&1 && eval "$(docker-machine env default)" 
 
type cf_completion > /dev/null 2>&1 && complete -C cf_completion cf

#
# Conveniences for dealing with docker-compose.
# If docker-machine, then env | grep DOCKER_ > ~/.docker/env.sh
#

DK_HELP='dk multipurpose hacky wrapper:
   dk up -d
   dk stop
   dk kill /container/
   dk rm /container/
   dk stats
   dk nets
   dk enter /container/
'

dk() {
    local file=docker-compose.yml
    local options
    local project=$( basename $PWD | sed s/-//g )
    local command
    local color=magenta

    while [[ $1 =~ ^- ]]; do
	case $1 in
	    --version|-v)
		docker-compose -v
		return
		;;
	    -h|--help)
		{ docker-compose 2>&1 ; echo; echo "$DK_HELP"; } | highlight ${color} '^[^ ].*:'
		return
		;;
	    --verbose) 
		options=$1
		shift
		options="$options --verbose"
		;;
	    --project|-p)
		shift
		project=$1
		shift
		options="$options -p $project"
		;;
	    --file|-f)
		shift
		file=$1
		shift
		;;
	esac
    done

    # try to infer yml file if none was given
    if [ ! -f $file ]; then
	local ymls
	{ shopt -s nullglob ; ymls=(*.yml); }
	case ${#ymls[@]} in
	    0) 
		echo "There is no $file"
		return;;
	    1) 
		echo
		echo ' *** ' Using ${ymls[0]} ' *** '
		echo
		file=${ymls[0]} ;;
	    *) 
		echo I can\'t guess which .yml to use: there are ${#ymls[@]}. Specify with --file.
		return;;
	esac
    fi
    
    options="$options -f $file"
    command=$1
    shift

    case $command in
       enter)
	   if [ -z "$1" ]; then
	       echo You must give a service to enter.
	       return
	   fi
            docker exec -it ${project}_${1}_1 bash
            ;;
        stats)
            local things=$( docker ps | perl -lane '/Up/ and print $F[-1]' )
            docker stats --no-stream=true $things | highlight ${color}  '^CONT.*$'
            ;;
        ps)
	    docker-compose $options ps | highlight ${color} 'Name.*|^--*$'
	    ;;
	nets)
	    local fmt="%-25s %s\n"
	    local ifc=vboxnet0
	    printf "$fmt" CONTAINER IPADDRESS | highlight ${color} '^C.*'
	    printf "$fmt" $ifc $(ifconfig $ifc | perl -ne '/inet[\s:]+([\.\d]+)/ and print $1')
	    for c in $( docker ps | perl -lane '/Up/ and print $F[-1]' ); do
		printf "$fmt" $c $( docker inspect $c | perl -ne '/"IPAddress": "([\.\d]+)"/ and print $1')
	    done
	    ;;
        *)
            docker-compose $options $command $@
            ;;
        esac
}
