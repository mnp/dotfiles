
# Noninteractive section

. ~/lib/shlib.bash


for d in /usr/man /usr/share/man /usr/local/man $HOME/perl5/man; do
    path_append MANPATH $d
done
export MANPATH

case $OSTYPE in
    linux*)  OS=linux;;
    darwin*) OS=darwin;;
esac

PATH=$HOME/bin:$HOME/hosts:/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin
path_append PATH $HOME/perl5/bin
path_append PATH $HOME/osbin
path_append PATH $HOME/workbin
path_append PATH $HOME/homebin
export PATH

# emacsclient and etags on osx
macemacs=/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14/
test -d $macemacs && path_append PATH $macemacs

# python executables installed by PIP
test -d ~/.local/bin && path_append PATH ~/.local/bin

for dir in $HOME/perl $HOME/perl5/lib/perl5 /usr/local/share/perl/* /usr/local/lib/perl/* /usr/local/lib/perl5/site_perl; do
    path_append PERL5LIB $dir
done
export PERL5LIB

# For MediaWiki client
export MVS_BROWSER=firefox

# Node
[ -d $HOME/.nvm ] && export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm

# Go lang
path_append PATH /usr/local/opt/go/libexec/bin

if [ -d $HOME/go ]; then
    GOPATH=$HOME/go
    path_append PATH $GOPATH/bin
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

    cd() {
	builtin cd "$@"
	nametab "${PWD##*/}"
    }
fi

BASH_COMPLETION=${BASH_COMPLETION:-/usr/local/opt/bash-completion/etc/bash_completion}
test -f $BASH_COMPLETION && . $BASH_COMPLETION

if type pyenv > /dev/null 2>&1; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
else
    path_append PATH /usr/local/Cellar/python/2.7.13_1/Frameworks/Python.framework/Versions/2.7/bin
fi

# eval "`pip completion --bash`"

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"
export LESS=-inXR

# happy terminal
if [ -d $HOME/terminfo ]; then
    export TERMINFO=$HOME/terminfo
    export TERM=xterm-color
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

INVERSE="\[\033[7m\]"


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
    alias glgp='git log --graph --decorate --pretty=oneline --abbrev-commit'
    alias gls='git log --stat'
    alias glf='git ls-files -t'
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

    if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
	__GIT_PROMPT_DIR=/usr/local/opt/bash-git-prompt/share
	source $__GIT_PROMPT_DIR/gitprompt.sh
    elif [ -f /usr/local/Cellar/bash-git-prompt/2.7.1/share/gitprompt.sh ]; then
        __GIT_PROMPT_DIR=/usr/local/Cellar/bash-git-prompt/2.7.1/share
        source $__GIT_PROMPT_DIR/gitprompt.sh
    elif [ -f ~/.git-prompt.sh ]; then
	source ~/.git-prompt.sh
    fi

    if [ -f ~/.git-completion.bash ]; then
	source ~/.git-completion.bash
    fi
fi

if type terraform > /dev/null 2>&1; then
    alias tf=terraform
fi

# disable ctrl-s software flow control
stty -ixon

my_prompt_command() {
:
}

# This works in iterm and xterm
function nametab() {
    echo -ne "\033]0;"$@"\007"
}

# no prompt command for console
case $TERM in
    xterm*)
        function nametab() {
            echo -ne "\033]0;"$@"\007"
        }
 	PS1BASE="${PS1COLOR}\u@\h${CLEAR}:${WHITE}\w${YELLOW}\$(__git_ps1)${CLEAR}\n\$ "
 	;;

    screen|vt100)
        function nametab() {
            echo -ne "\033k"$@"\033\\"
        }
        # Git sets PROMPT_COMMAND above. Add to it.
	PS1BASE="${PS1COLOR}\u@\h${CLEAR}:${WHITE}\w${YELLOW}\$(__git_ps1)${CLEAR}\n\$ "
	;;

    *)
	PROMPT_COMMAND=''
	PS1BASE='\u@\h:\w\n\$ '
	;;
esac

if type git > /dev/null 2>&1; then
    alias tf=terraform
fi

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

# # enable color support of ls
if type dircolors > /dev/null 2>&1; then
     if [[ "$TERM" != "dumb" && -d ~/prj/dircolors-solarized ]]; then
 	eval `dircolors ~/prj/dircolors-solarized/dircolors.256dark`
	export LS_OPTIONS=--color=auto
     fi
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
	    *.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp|*.wav|*.mp3|*.mkv|\
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
	    *.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp|*.mkv) mplayer "$1";;
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
    test -z "$1" && set .
    m "$1"/"$(ls -rt "$1"|tail -1)"
}

# edit the last "ls -lrt"
ert ()
{
    test -z "$1" && set .
    e "$1"/"$(ls -rt "$1"|tail -1)"
}

lrt() {
    if [ -z $1 ]; then
	set .
    elif [ -L $1 ]; then
	local link=$(readlink $1)
	case "$link" in
	    /*) set "$link";;
	    *) set $(dirname $1)"$link";;
	esac
    fi
    ls -lrt $1
}

lstoday()
{
    ls -l $@ | egrep "$(date '+%b %_d')"
}

logcmd()
{
    eval "$@" 2>&1 | tee log
}

# saves to clipboard
rsatoken()
{
    stoken | tr -d '\n' | pbcopy
}

type ack    > /dev/null 2>&1 && alias ack='ack-grep'

alias l='ls $LS_OPTIONS'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias la='ls $LS_OPTIONS -lA'
alias lh='ls -lhS'

alias jc='jq -C . $@ | less -r'

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
alias g='grep -i --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias jag='ag --java'

alias gd='./gradlew'

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

function pc ()
{
    nc -z $(echo $@ | sed 's/:/ /g')
    echo $?
}

function fj ()
{
    find ${1:-.} -type f -name \*.java
}

#
# remember what we were doing and where
# todo? keep some k,v in that file for notes or context
# or maybe use emacs' .dir-locals.el?  (work) would be okay..
#
work() {
    if [ -n "$1" ]; then
	cd $1
	echo WORK=\"`pwd`\" > ~/.work
    elif [ -f ~/.work ]; then
	.  ~/.work
	cd $WORK
    else
	echo you have to set work
    fi
}

#
# ..   - Does a "cd .."
# .. 3 - Does a "cd ../../.."
#
function .. ()
{
    local arg=${1:-1};
    while [ $arg -gt 0 ]; do
	builtin cd .. >&/dev/null;
	arg=$(($arg - 1));
    done
    nametab `basename $PWD`;
}
# function cd_dot_dot
# alias ..=cd_dot_dot

clean ()
{
    if [ $# -lt 1 ]; then
	echo ,* *~ .*~ \#*\#  *.pyc __pycache__
	/bin/rm -rf ,* *~ .*~ \#*\# *.pyc  __pycache__
    else
	for i in $@; do
	    if [ -d "$i" ]; then
		echo ---- Cleaning $i ----
		( builtin cd $i
		  echo ,* *~ .*~ \#*\#  *.pyc __pycache__
		  /bin/rm -rf ,* *~ .*~ \#*\#  *.pyc __pycache__ )
	    else
		echo Huh?
	    fi
	done
    fi
}

# moved to ~/bin/p
# # ps grep
# p ()
# {
#     if [ $# -lt 1 ]; then
# 	ps faux | $PAGER
#     else
# 	local pid=$(pgrep -f $1)
# 	if [[ -n $pid ]]; then
# 	    ps lww -p $pid
# 	else
# 	    echo None found
# 	fi
#     fi
# }

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


# <@keck> less for tab-delimited files
aless(){ perl -e 'BEGIN{$f=shift;%cs=();} open(IN,"<", $f); while(<IN>){$c=0; map{ $s=length;$cs{$c}=($s>$cs{$c})?$s:$cs{$c}; $c++;}split /\t/;}END{$cs{0}++;$x=join
              ",",map{($_>0)?($cs{$_}=$cs{$_}+$cs{$_-1}+1):$cs{$_}; $cs{$_}} sort{$a<=>$b}keys %cs;system("less -S -x$x $f");}' $1; }
#
# select java versions; provide java7 and java8 aliases to switch on
# the fly
#

# if [ -x /usr/libexec/java_home ]; then
#     # osx-ish
#     export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
#     export JAVA_7_HOME=$(/usr/libexec/java_home -v1.7)
#
#     rejava()
#     {
# 	export JAVA_HOME="$1"
# 	echo JAVA_HOME is now "$1"
#
# 	# normalize paths and then append
#  	[[ $PATH    =~ (.*):$JAVA_7_HOME/bin:(.*) ]] &&    PATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
#  	[[ $MANPATH =~ (.*):$JAVA_7_HOME/man:(.*) ]] && MANPATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
#  	[[ $PATH    =~ (.*):$JAVA_8_HOME/bin:(.*) ]] &&    PATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
#  	[[ $MANPATH =~ (.*):$JAVA_8_HOME/man:(.*) ]] && MANPATH=${BASH_REMATCH[1]}:${BASH_REMATCH[2]}
# 	export PATH=$PATH:$JAVA_HOME/bin
# 	export MANPATH=$MANPATH:$JAVA_HOME/man
#     }
#
#     java7()
#     {
# 	rejava $JAVA_7_HOME
# 	PS1=${MAGEN}7${CLEAR}:$PS1BASE
#     }
#
#     java8()
#     {
# 	rejava $JAVA_8_HOME
# 	PS1=${MAGEN}8${CLEAR}:$PS1BASE
#     }
#
# elif [ -x /usr/sbin/update-java-alternatives ]; then
#
#     # linux-ish /usr/sbin/update-java-alternatives
#     java7()
#     {
#        sudo update-java-alternatives -s java-7-oracle
#        PS1=${MAGEN}7${CLEAR}:$PS1BASE
#     }
#
#     java8()
#     {
#        sudo update-java-alternatives -s java-8-oracle
#        PS1=${MAGEN}8${CLEAR}:$PS1BASE
#     }
#
# fi

## set default at work at least
#if [ -d ~/workbin ]; then
#    java8
#fi

# # Avoid errors from any UTF8 in code; I guess eclipse can add them.
# export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8

if [ -d ${HOME}/perl5/lib/perl5/local/lib.pm ]; then
    # added by duckpan installer
    eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)
fi



# if type docker-machine > /dev/null 2>&1 ; then
#     eval "$(docker-machine env default)"
#     echo docker-machine: $(docker-machine active) $(docker-machine status)
#     alias dm=docker-machine
#
#     function dkr()
#     {
# 	docker-compose kill $1
# 	docker-compose rm -f $1
# 	docker-compose up -d $1
# 	echo ' ---- TAILING -----' $1 logs
# 	docker-compose logs --follow $1
#     }
#     [[ $DOCKER_HOST =~ ([0-9.]+) ]] && export DM=${BASH_REMATCH[1]}
#
#     function jc()
#     {
# 	curl -s $DM:$1 | jq .
#     }
# fi

alias dk=docker-compose

# Last Container operations
lc ()
{
    last=$(docker ps -q | head -1)
    case $1 in
        sh)   docker exec -it $last sh ;;
        kill) docker kill $last ;;
        *) echo 'Usage: lc sh | kill' ;;
    esac
}

# Last Image operations
li () {
    echo NIY
    return

    last=$(docker images -q | head -1)
    case $1 in
	tag)  docker tag $last $1;;
	push) docker push $(docker images | awk '!/^REPO/ { print $1 ":" $2; exit; }') ;;
	bash) docker run --entrypoint bash -it $last;;
	sh)   docker run --entrypoint sh -it $last;;
	*) echo 'Usage: li tag FOO | push | sh | bash'
	   ;;
    esac
}


function dtag() {
    docker tag $(docker images -q | head -1) $1
}

function dpush() {
    tag=$(docker images | awk '!/^REPO/ { print $1 ":" $2; exit; }')
    echo Pushing $tag ...
    docker push $tag
}

function dbash () {
    docker run --entrypoint bash -it $(docker images -q | head -1)
}


if [ -d $HOME/workbin ]; then
    . ~/.bashrc-work
fi

if [ -d $HOME/homebin ]; then
    . ~/.bashrc-home
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
