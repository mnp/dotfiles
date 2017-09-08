# bash utilities library

# path_prepend/append PATHVAR DIR

path_append()
{
    if [ -d "$2" ] && [[ ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1+=:$2"
    fi
}

path_prepend()
{
    if [ -d "$2" ] && [[ ! ${!1} =~ (^|:)$2(:|$) ]]; then
        eval "$1=$2:\$$1"
    fi
}

note () 
{
    echo ===============================================================
    echo $@
    echo ===============================================================
}

