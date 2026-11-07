#!/usr/bin/env zsh
# Activate in the current shell before configuring a build:
#   source ~/.config/zsh/z/compile.zsh
# On macOS, use Homebrew libc++ and libunwind for builds.

function() {
    local brew_prefix llvm_prefix sdk cmake_prefixes
    local -aU prefixes pkg_paths

    if ! (( $+commands[brew] )); then
        print -u2 'compile.zsh: Homebrew is not on PATH'
        return 1
    fi
    brew_prefix=${BREW:-${HOMEBREW_PREFIX:-}}
    [[ -n $brew_prefix ]] || brew_prefix=$(brew --prefix) || return
    llvm_prefix="$brew_prefix/opt/llvm"
    if [[ ! -x "$llvm_prefix/bin/clang" ]]; then
        print -u2 'compile.zsh: install the compiler with: brew install llvm'
        return 1
    fi

    export CC="$llvm_prefix/bin/clang"
    export CXX="$llvm_prefix/bin/clang++"
    export AR="$llvm_prefix/bin/llvm-ar"
    export RANLIB="$llvm_prefix/bin/llvm-ranlib"
    # Re-sourcing should not duplicate paths or flags.
    path=("$llvm_prefix/bin" "${(@)path:#$llvm_prefix/bin}")
    export PATH

    [[ " ${CPPFLAGS:-} " == *" -I$brew_prefix/include "* ]] ||
        export CPPFLAGS="-I$brew_prefix/include${CPPFLAGS:+ $CPPFLAGS}"
    [[ " ${LDFLAGS:-} " == *" -L$brew_prefix/lib "* ]] ||
        export LDFLAGS="-L$brew_prefix/lib${LDFLAGS:+ $LDFLAGS}"
    pkg_paths=("$brew_prefix/lib/pkgconfig" "$brew_prefix/share/pkgconfig"
               "${(@s/:/)PKG_CONFIG_PATH}")
    export PKG_CONFIG_PATH="${(j/:/)pkg_paths}"
    # CMake's environment variable uses colon separators on Unix.
    cmake_prefixes=${CMAKE_PREFIX_PATH//;/:}
    prefixes=("$brew_prefix" "$llvm_prefix" "${(@s/:/)cmake_prefixes}")
    export CMAKE_PREFIX_PATH="${(j/:/)prefixes}"

    if [[ $OSTYPE == darwin* ]]; then
        sdk=$(xcrun --show-sdk-path) || return
        export SDKROOT="$sdk"
        local runtime_flags="-L$llvm_prefix/lib/c++ -Wl,-rpath,$llvm_prefix/lib/c++ -L$llvm_prefix/lib/unwind -Wl,-rpath,$llvm_prefix/lib/unwind -lunwind"
        [[ " ${LDFLAGS:-} " == *" -L$llvm_prefix/lib/c++ "* ]] ||
            export LDFLAGS="$runtime_flags${LDFLAGS:+ $LDFLAGS}"
    fi
}
