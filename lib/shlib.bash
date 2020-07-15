# bash utilities library

fatal() { echo `date -u` $0 Fatal Error - $@; exit 1; }
warn()  { echo `date -u` $0 Warning - $@; }
info()  { echo `date -u` $0 Info - $@; }

cat_help()
{
    sed -rn 's/^### ?//;T;p' "$0"
}

# path_prepend/append PATHVAR DIR

path_append()
{
    # [ -d "$2" ] && 
    if [[ ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1+=:$2"
    fi
}

path_prepend()
{
    # [ -d "$2" ] && 
    if [[ ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1=$2:\$$1"
    fi
}

note () 
{
    echo ===============================================================
    echo $@
    echo ===============================================================
}

