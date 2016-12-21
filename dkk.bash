#!/bin/bash

#
# Conveniences for dealing with docker-compose.
#
# 1. Load this file into your bash or add to your .bashrc:
#
#     $ source dkk.bash
#
# 2. Use "dk" instead of "docker-compose" like an alias.  If you ever use the "-f" option,
# it will keep track of it for you and even manage the prompt. You won't need to give "-f" again.
#
#

dk () {

    usage() {
	docker-compose 2>&1
	echo
	echo 'dk - multipurpose hacky wrapper

Usage:
   dk ... any regular docker-compose args ...
'
	exit 1
    }

    file=docker-compose.yml
    command=""
    options=""

    while [[ $1 =~ ^- && $# -gt 2 ]]; do
	case $1 in
	    -h|--help)
		usage
		return
		;;
	    --file|-f)
		shift
		file=$1
		shift
		;;
	esac
    done

    if [[ $# -eq 0 ]]; then
	usage
	return
    fi

    options="$options -f $file"
    command=$1
    shift

    services() {
	docker-compose $options config --services
    }

    echo $file
    echo $project
    return

    case $command in
	reset)
	    docker-compose $options kill
	    docker-compose $options rm -f
	    docker-compose $options up -d
	    ;;
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
	    # fall through any other subcommands to the expert
	    docker-compose $options $command $@
	    ;;
    esac


}

dk $@
