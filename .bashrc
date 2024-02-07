# Noninteractive section
echo bashrc noninteractive section

. ~/lib/shlib.bash

set_have_cmd_vars git bat brew terraform gzcat zcat oocalc kubectl docker lesspipe

for d in /usr/man /usr/share/man /usr/local/man $HOME/perl5/man; do
    path_append MANPATH $d
done
export MANPATH
export PATH

# emacsclient and etags on osx, needs to be before /usr/local
macemacs=/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14/
test -d $macemacs && path_append PATH $macemacs

PATH=/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin

# OSX. TODO: incorporate
if [ -x /usr/libexec/path_helper ]; then
    eval $(/usr/libexec/path_helper)
fi

# osx
test -d /opt/local/bin && path_append PATH /opt/local/bin 
#test -d /Library/Frameworks/Mono.framework/Home/bin && path_append PATH /Library/Frameworks/Mono.framework/Home/bin

# pony
#if [ -d ~/.local/share/ponyup/bin ]; then
#    export CC=gcc
#    path_append PATH ~/.local/share/ponyup/bin
#fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

test -d $HOME/.local/bin && path_append PATH $HOME/.local/bin

test -d $HOME/.cask/bin && path_append PATH $HOME/.cask/bin

# python executables installed by PIP
test -d ~/.local/bin && path_append PATH ~/.local/bin

# node
test -d ~/node_modules/.bin && path_append PATH ~/node_modules/.bin

# golang
test -d $HOME/go/bin && path_append PATH $HOME/go/bin

# rust, cargo, etc
test -r $HOME/.cargo/env && . $HOME/.cargo/env
test -f $HOME/.cargo/bin && path_append PATH "$HOME/.cargo/bin"

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
if [ -d /usr/local/opt/go/libexec/bin ]; then
    path_append PATH /usr/local/opt/go/libexec/bin
fi

if [ -d $HOME/go ]; then
    GOPATH=$HOME/go
    path_append PATH $GOPATH/bin
fi
export GOPATH

path_prepend PATH $HOME/bin
path_prepend PATH $HOME/perl5/bin
path_prepend PATH $HOME/osbin
path_prepend PATH $HOME/workbin
path_prepend PATH $HOME/homebin


unset MIBS MIBDIRS

# Interactive section
# If not running interactively, don't do anything

#[ -z "$PS1" ] && return
echo bashrc interactive section

case $BASH_VERSION in
    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    3*) shopt -s checkwinsize;;
esac

# various formatters use these
export COLUMNS LINES

if [ -f $HOME/etc/mylib.bc ]; then
    export BC_ENV_ARGS='-l $HOME/etc/mylib.bc'
fi

# OSX
if $have_brew; then
    # iterm
    nametab() { echo -ne "\033]0;"$@"\007"; }

    cd() {
	builtin cd "$@"
	nametab "${PWD##*/}"
    }
fi


BASH_COMPLETION=${BASH_COMPLETION:-/usr/local/opt/bash-completion/etc/bash_completion}
test -f $BASH_COMPLETION && . $BASH_COMPLETION

# eval "`pip completion --bash`"

# make less more friendly for non-text input files, see lesspipe(1)
if $have_lesspipe; then
    eval "$(lesspipe)"
    # TODO: bat and m integrate
fi
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
LTGRNUL="\033[0;32;4m"

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

if $have_git; then
    alias gsm=' git status --untracked-files=no --ignore-submodules'
    alias gds=' git diff --stat'
    alias gdp=' git diff        HEAD~1 --'
    alias gdps='git diff --stat HEAD~1 --'
    alias glgp='git log --graph --decorate=full --pretty=oneline --abbrev-commit'
    alias gls=' git log --stat'
    alias glf=' git ls-files -t'
    alias grpo='git remote prune origin'

    # 'git grep'
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

    # from: https://github.com/magicmonty/bash-git-prompt

    # GIT_PROMPT_ONLY_IN_REPO=1
    # GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status
    # GIT_PROMPT_IGNORE_SUBMODULES=1 # uncomment to avoid searching for changed files in submodules
    # GIT_PROMPT_WITH_VIRTUAL_ENV=0 # uncomment to avoid setting virtual environment infos for node/python/conda environments
    # GIT_PROMPT_SHOW_UPSTREAM=1 # uncomment to show upstream tracking branch
    GIT_PROMPT_SHOW_UNTRACKED_FILES=no # can be no, normal or all; determines counting of untracked files
    # GIT_PROMPT_SHOW_CHANGED_FILES_COUNT=0 # uncomment to avoid printing the number of changed files
    # GIT_PROMPT_STATUS_COMMAND=gitstatus_pre-1.7.10.sh # uncomment to support Git older than 1.7.10
    # GIT_PROMPT_START=...    # uncomment for custom prompt start sequence
    # GIT_PROMPT_END=...      # uncomment for custom prompt end sequence
    # as last entry source the gitprompt script
    # GIT_PROMPT_THEME=Custom # use custom theme specified in file GIT_PROMPT_THEME_FILE (default ~/.git-prompt-colors.sh)
    # GIT_PROMPT_THEME_FILE=~/.git-prompt-colors.sh
    # GIT_PROMPT_THEME=Solarized # use theme optimized for solarized color scheme
 
    if [ $SHLVL -gt 1 ]; then
        GIT_PROMPT_END=" ${INVERSE}SUBSHELL${CLEAR} \$ "
    fi

    if [ -f "$HOME/prj/bash-git-prompt/gitprompt.sh" ]; then
        __GIT_PROMPT_DIR="$HOME/prj/bash-git-prompt"
        source "$__GIT_PROMPT_DIR/gitprompt.sh"
    fi

    if [ -f ~/.git-completion.bash ]; then
	source ~/.git-completion.bash
    fi
fi

if $have_tf; then
    alias tf=terraform
fi

# disable ctrl-s software flow control
stty -ixon

# This works in iterm and xterm
function nametab() {
    echo -ne "\033]0;"$@"\007"
}

#
# historystuff
#
HISTSIZE=10000
HISTTIMEFORMAT='%F %T '
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

if type beep-media-player > /dev/null 2>&1; then
    AUPLAY=beep-media-player
else
    AUPLAY='echo need to set player'
fi

export PAGER=less
export EDITOR='fe'              # emacsclient
export VISUAL='fe'              # emacsclient
export LANG=C

# Enable color support of ls
export LS_OPTIONS=--color=auto

if [ -f .dircolors-solarized-256dark ]; then
    source .dircolors-solarized-256dark
    export LS_OPTIONS=--color=auto
else
    export LSCOLORS="Gxfxcxdxbxegedabagacad"
fi

if $have_gzcat; then
    ZCAT=gzcat
elif $have_zcat; then
    ZCAT=no
else
    echo note: no zcat found
fi

if $have_oocalc; then
    OFFICE=yes
fi

if $have_bat; then
    PREPAGER=bat
else
    PREPAGER=less
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
            if [ -n "$2" ]; then
	        if [ $ZCAT != no ]; then
		    gzcat "$1" | tar -xvf - -O "$2" | $PAGER
	        else
		    tar -zxvf "$1" -O "$2" | $PAGER
	        fi
            else
	        if [ $ZCAT != no ]; then
		    gzcat "$1" | tar tvf - | $PAGER
	        else
		    tar ztvf "$1" | $PAGER
	        fi
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

    if [[ $OSTYPE == darwin* ]]; then
	case "$1" in
	    *.wmv|*.mpg|*.WMV|*.rm|*.MPG|*.avi|*.AVI|*.mp4|*.3gp|*.wav|*.mp3|*.mkv|\
	    *.pnm|*.pbm|*.jpg|*.jpeg|*.JPG|*.gif|*.GIF|*.tif|*.tiff|\
	    *.doc|*.DOC|*.docx|\
	    *.odp|*.ppt|*.pptx|*.PPT|\
	    *.ods|*.xls|*.XLS|*.xlsx|*.odt|\
	    *.djvu|*.ps|*.pdf|*.PDF|\
	    *.png|*.PNG|*.bmp|*.BMP|*.jpg|*.JPG|*.gif|*.GIF|*.html) open "$1";;
	    *) $PREPAGER "$1";;
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
	    *) $PREPAGER "$1";;
	esac
    fi
}

# more the last "ls -rt"
mrt ()
{
    test -z "$1" && set .
    m "$1"/"$(ls -rt "$1"|tail -1)"
}

# move most recent file in directory ($1) to path or dir ($2)
mvrt () 
{
    test -z "$1" && set .
    mv "$1"/"$(ls -rt "$1"|tail -1)" $2
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
    
    local beg=$(date +%s)
    (
        date "+Logcmd started: %Y-%m-%d %H:%M:%S %Z";
        eval "$@" 2>&1;
        date "+Logcmd finished: %Y-%m-%d %H:%M:%S %Z"
    ) | tee log
    local end=$(date +%s)
    echo Logcmd wrote to ./log, $[end - beg] sec elapsed
}

# saves to clipboard
rsatoken()
{
    stoken | tr -d '\n' | pbcopy
}

$have_ack && alias ack='ack-grep'

alias l='ls $LS_OPTIONS'
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias la='ls $LS_OPTIONS -lA'
alias lh='ls -lhS'

function jc() { jq -C . $@ | less -r; }
function cj() { curl -s $1 | jq -C . | less -r; }

alias mlp='m `ls -rt /tmp/*pdf|tail -1`'
alias rm='rm -i'
alias Rm='command rm -f'
alias cp='cp -i'
alias mv='mv -i'
alias t=tail

if [ $OSTYPE == linux-gnu ]; then
  alias acs='apt search'
  alias acss='apt show'
  alias agu='sudo apt update'
  alias agi='sudo apt install'
fi

alias bye='exit'
alias y='echo Oops\!'
alias pf='perldoc -f'
alias r='fc -s'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias pg='pgrep -aif'
alias jag='ag --java'
alias rag='ag --rust'

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

# See https://github.com/tecosaur/emacs-everywhere
alias ee='emacsclient --eval "(emacs-everywhere)"'

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
# Remember what we were doing and where. I like to keep two projects like this, ie work and workb
# todo? keep some k,v in that file for notes or context or maybe use emacs' .dir-locals.el? (work)
# would be okay..
#
work() {
    if [ -n "$1" ]; then
	cd $1
	echo WORK=\"`pwd`\" > ~/.work
        WORK=`pwd`
    elif [ -f ~/.work ]; then
	.  ~/.work
	cd $WORK
    else
	echo you have to set work
    fi
}

workb() {
    if [ -n "$1" ]; then
	cd $1
	echo WORKB=\"`pwd`\" > ~/.workb
        WORKB=`pwd`
    elif [ -f ~/.workb ]; then
	.  ~/.workb
	cd $WORKB
    else
	echo you have to set workb
    fi
}

# Just like work but called task. Two contexts are needed.
task() {
    if [ -n "$1" ]; then
	cd $1
	echo TASK=\"`pwd`\" > ~/.task
        TASK=`pwd`
    elif [ -f ~/.task ]; then
	.  ~/.task
	cd $TASK
    else
	echo you have to set task
    fi
}

taskb() {
    if [ -n "$1" ]; then
	cd $1
	echo TASKB=\"`pwd`\" > ~/.taskb
        TASKB=`pwd`
    elif [ -f ~/.taskb ]; then
	.  ~/.taskb
	cd $TASKB
    else
	echo you have to set taskb
    fi
}

export TASK TASKB WORK WORKB

# read context another shell might have created
test -f ~/.work && source ~/.work
test -f ~/.task && source ~/.task
test -f ~/.workb && source ~/.workb
test -f ~/.taskb && source ~/.taskb

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
    emacsclient --no-wait $(builtin type -path "$1")  ## not useful:  _xw_sub $EDITOR $1
}

# cat which
cw ()
{
    _xw_sub cat $1
}

# Allow ^ those to complete on execution path
complete -c mw
complete -c ew
complete -c cw

# edit find
ef ()
{
    found=($(find . -name $1 -print))
    case ${#found[@]} in
        1) echo editing $found; $EDITOR $found ;;
        0) echo none found, no action;;
        *) echo many found, no action;;
    esac
}

# pager find
mf ()
{
    found=($(find . -name $1 -print))
    case ${#found[@]} in
        1) echo editing $found; $PAGER $found ;;
        0) echo none found, no action;;
        *) echo many found, no action;;
    esac
}

_xw_sub ()    # _xw_sub ACTION FILE
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

# See http://wttr.in/:help especially :bash_function
wttr ()
{
    curl 'wttr.in?1'
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

# if [ -d ${HOME}/perl5/lib/perl5/local/lib.pm ]; then
#     # added by duckpan installer
#     eval $(perl -I${HOME}/perl5/lib/perl5 -Mlocal::lib)
# fi



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

if $have_kubectl; then
    prompt_callback() {
        local cc

        if cc=$(kubectl config current-context); then
            if [[ $cc == kind-kind || $cc =~ "mitch" ]]; then
                echo -n " ${LTGRNUL}$cc${CLEAR}"
            else
                echo -n " ${LTRED}${INVERSE}$cc${CLEAR}"
            fi
        else
            echo -n " (nok8s)"
        fi
    }

    alias k=kubectl
    alias kc-disk='kc get cm,pv,pvc,crd --field-selector metadata.namespace!=kube-system -A'

    # --field-selector metadata.namespace!=kube-system
    kga() { kubectl get pod,service,deployment,replicaset,pvc,cm,crd $@; }
    kgp() { kubectl get pods --sort-by '{.metadata.name}' $@; }
    kgpi() { kubectl get pods --sort-by '{.metadata.name}' $@ -o custom-columns='NAME:.metadata.name,STATUS:.status.phase,IMAGE:.spec.containers[0].image,PULL_SECRETS:.spec.imagePullSecrets[*].name'; }
    kgpw() { kubectl get pods $@ -o wide; }
    kdp() { kubectl describe pod $@; }
    kgs() { kubectl get services --sort-by '{.metadata.name}' $@; }
    kge() { kubectl get events --sort-by='.metadata.creationTimestamp' $@; }

    alias kcns=kubens

    kcl() { kubectl logs -f pod/$(kc-getpod $1);  }
    kcs() {
        local pod="$(kubectl getpod ${1:?'Pod expected'})"
        shift
        kubectl exec -it "$pod" -- ${@:-sh};
    }

    source <(kubectl completion bash)
    complete -F __start_kubectl k

    if [ -d ${HOME}/.krew/bin ]; then
        path_append PATH "${HOME}/.krew/bin"
    fi
fi

if $have_docker; then
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
        local image=${1:-$(docker images -q | head -1)}
        docker run --entrypoint bash -it $image
    }

fi

if [ -r ~/.bashrc-work ]; then
    . ~/.bashrc-work
fi

if [ -r ~/.bashrc-home ]; then
    . ~/.bashrc-home
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/__tabtab.zsh ]] && . ~/.config/tabtab/__tabtab.zsh || true



alias docker-minikube='eval $(minikube -p minikube docker-env)'

. "$HOME/.cargo/env"

if type pyenv > /dev/null 2>&1; then
    export PYENV_ROOT="$HOME/.pyenv"
    command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi
