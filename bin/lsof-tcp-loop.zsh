#!/usr/bin/env zsh

autoload -Uz bin-abort ba-err echoer trim-whitespace
trap 'bin-abort' ERR

function _lsof_tcp_loop() {
	local i=1

	local sleepTime=${1:-10}
	[[ $# > 0 ]] && shift

	while true; do
		echo "refresh #$i"
		echo

		output=$(lsof -a -i4 -i6 -sTCP:ESTABLISHED \
			| awk -F$' ' '{print $1,$2,$3,$4,$5,$8,$9}' \
			| column -t)
		clear
		echo "$output"

		i=$(( i+1 ))

		echo
		echo "refreshing in $sleepTime ..."

		sleep "$sleepTime"
	done
}

#set -x
_lsof_tcp_loop "$@"
#set +x
