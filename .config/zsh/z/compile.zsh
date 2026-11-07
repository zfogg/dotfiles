#!/usr/bin/env zsh
# vim: set fdm=marker:
#   ~/.config/zsh/z/compile.zsh

function() {
    local cc cxx cflags cppflags ldflags

    # CC CXX {{{
    cc=clang
    cxx=clang++
    # }}}


    # CFLAGS CPPFLAGS {{{
    cflags=(
    )
    cppflags=(
    )
    # }}}


    # LDFLAGS {{{
    ldflags=(
    )
    # }}}


    # Set and export global uppercase variables {{{
    export CC="$cc"
    export CXX="$cxx"
    export CFLAGS="${(j: :)cflags}"
    export CPPFLAGS="${(j: :)cppflags}"
    export LDFLAGS="${(j: :)ldflags}"
    # }}}
}
