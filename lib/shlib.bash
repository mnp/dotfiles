# bash utilities library

fatal() { echo `date -u` $0 Fatal Error - $@; exit 1; }
warn()  { echo `date -u` $0 Warning - $@; }
info()  { echo `date -u` $0 Info - $@; }

set_have_cmd_vars() {
    local cmd
    for cmd in "$@"; do
        if command -v $cmd &> /dev/null; then
            eval "have_$cmd=true"
        else
            eval "have_$cmd=false"
        fi
    done
}

cat_help()
{
    sed -rn 's/^### ?//;T;p' "$0"
}

# path_prepend/append PATHVAR DIR

path_append()
{
    if [[ -d "$2" && ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1+=:$2"
    fi
}

path_prepend()
{
    if [[ -d "$2" && ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1=$2:\$$1"
    fi
}

note () 
{
    echo ===============================================================
    echo $@
    echo ===============================================================
}

