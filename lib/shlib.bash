# bash utilities library

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

