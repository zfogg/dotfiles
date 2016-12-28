#!/bin/zsh


[[ ! -z $DEBUG ]] && set -x


ext="${2:-wav}"

find "${1:-.}" \
    -type f \
    -name '*.'"$ext" \
    -exec printf '%s\n' '{}' + \
| {
    while read w; do
        echo 'START '"$w"
        sox "$w" -n spectrogram
        mv 'spectrogram.png' "$(dirname "$w")"'/'"$(basename -s '.'"$ext" "$w")"'.png'
        echo 'END '"$w"
    done
}


[[ ! -z $DEBUG ]] && set +x
