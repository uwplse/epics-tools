#!/bin/bash
set -e

# TODO: check for alloy4.2_2015-02-22.  Ordinary alloy4.2 contains a much
# slower build of libminisat.so, which can't actually solve anything in a
# reasonable amount of time.

err_count=0

do_temp() {
    local ext=$1
    local template="tmp.XXXXXXXXXXXX"

    if [[ -n "$ext" ]]; then
        template="tmp-XXXXXXXXXXXX.$ext"
    fi

    local file=$(mktemp "$template")
    cat >"$file"
    echo "$file"
}

err() {
    echo $'\x1b[31merror:\x1b[0m '"$@"
    err_count=$((err_count + 1))
}

info() {
    echo "$@"
}

advise() {
    echo $' \x1b[34m*\x1b[0m '"$@"
}

groupadvise() {
    if group_has_errors; then
        advise "$@"
    fi
}

ok() {
    echo $' \x1b[32m*\x1b[0m '"$@"
}

group() {
    echo
    echo $' === '"Checking for $1"
    group_err_count=$err_count
}

group_has_errors() {
    [[ "$err_count" -gt "$group_err_count" ]]
}

osx_host() {
    [[ "$(uname)" == "Darwin" ]]
}

found_var_name() {
    local kind=$1
    local name=$2
    name=${name//\//_}
    name=${name//./_}
    echo -n "found_${kind}_${name}"
}

record() {
    local var=$(found_var_name "$1" "$2")
    eval "${var}=1"
}

found() {
    local var=$(found_var_name "$1" "$2")
    [[ "${!var}" == "1" ]]
}


check_binary() {
    if ! type "$1" 2>/dev/null; then
        err "Couldn't find binary '$1' ($2)"
    else
        ok "Found binary '$1' ($2)"
        record binary "$1"
    fi
}

check_rosette() {
    local file=$(do_temp rkt <<EOF
#lang s-exp rosette
EOF
)
    if racket "$file" 2>/dev/null; then
        ok "Found Rosette"
        record rosette rosette
    else
        err "Couldn't find Rosette"
        info "Follow the installation instructions at: https://github.com/emina/rosette"
    fi
    rm -f "$file"
}

# check_file <basename> <desc> <var name>
check_file() {
    if [[ -f "${!3}" ]]; then
        ok "Found '$(basename ${!3})' ($2)"
        record file "$1"
    else
        err "Couldn't find '$1' ($2)"
        if [[ -n "${!3}" ]]; then
            info "\$$3 is set but does not refer to a file"
            info "\$$3 = ${!3}"
        else
            info "Set \$$3 to the path to $1"
        fi
    fi
}

# check_dir <short name> <desc> <var name>
check_dir() {
    if [[ -d "${!3}" ]]; then
        ok "Found '$(basename ${!3})/' ($2)"
        record dir "$1"
    else
        err "Couldn't find '$1/' ($2)"
        if [[ -n "${!3}" ]]; then
            info "\$$3 is set but does not refer to a directory"
            info "\$$3 = ${!3}"
        else
            info "Set \$$3 to the path to $1/"
        fi
    fi
}

# check_var <var name> <desc>
check_var() {
    if [[ -n "${!1}" ]]; then
        ok "\$$1 ($2) is set"
        record var "$1"
    else
        err "\$$1 ($2) is not set"
    fi
}

try_header() {
    echo "#include <$1>" | gcc -x c - -c -o /dev/null 2>/dev/null
}

check_header() {
    if try_header $1; then
        ok "Found '$1' ($2)"
        record header "$1"
    else
        err "Couldn't find '$1' ($2)"
    fi
}

try_library() {
    echo "int main() {}" | gcc -x c - -o /dev/null -l"$1" 2>/dev/null
}

check_library() {
    if try_library $1; then
        ok "Found 'lib$1' ($2)"
        record library "$1"
    else
        err "Couldn't find 'lib$1' ($2)"
    fi
}

check_coq_version() {
    local TARGET="$1"
    local VER="$(coqc --version | grep -o 'version \S*')"
    if [[ "$VER" == "version $TARGET" ]]; then
        ok "Found coqc version $TARGET"
    else
        err "Found coqc $VER; expected $TARGET"
    fi
}

neutrons=$(dirname "$0")

info "Found project directory: $neutrons"

if [[ ! -f "$neutrons/vars.sh" ]]; then
    cat >"$neutrons/vars.sh" <<EOF
# Environment variables for development
export EPICS_HOST_ARCH=
export EPICS_BASE=
export SUPPORT=
EOF
    info "Created '$neutrons/vars.sh'"
    info "You must edit vars.sh to fill in appropriate values for each variable"
    info "Run \`. vars.sh\` to set up the environment before working"
fi

group Haskell
check_binary stack "Haskell Stack build system"

group Rosette
check_binary racket "Racket interpreter"
check_rosette

group Python
check_binary python3 "Python 3 interpreter"

group "EPICS build dependencies"
check_binary curl "command-line file downloader"
check_header readline/readline.h "'libreadline' development files"
if osx_host; then
    check_library l "'flex' lexer generator support library"
else
    check_library fl "'flex' lexer generator support library"
fi

group Misc
check_binary tree "tree-format directory lister"

[ "$err_count" -eq 0 ]
