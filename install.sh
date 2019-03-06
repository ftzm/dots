#!/usr/bin/env bash

# ----------------------------------------------------------------------
# Globals

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
SCRIPT_NAME=$(basename "$0")
SCRIPT_PATH="${SCRIPT_DIR}/${SCRIPT_NAME}"
SCRIPT_OWNER=$(stat -c '%U' "${SCRIPT_PATH}")

UNSET="\e[0m"
GREY="\e[90m"
RED="\e[91m"
GREEN="\e[92m"
YELLOW="\e[93m"

declare -A STATUS_COLORS=(
    [empty]=$GREEN
    [occupied]=$RED
    [wrong]=$YELLOW
    [correct]=$GREY
)

AUTOCONFIRM=
DRYRUN=
MODULES_FILE=
MODULES=

declare -a MODULE_OBJS
declare -a BACKUPS_MADE

# ----------------------------------------------------------------------
# Utility functions

join_by () { local IFS="$1"; shift; echo "$*"; }

title () { echo -e "$1\n${1//?/${2:--}}\n";}

prompt () {
    local message=$1
    local pass=$2
    read -p "${message}. Enter y to continue: " -n 1 -r
    echo    # move to a new line
    if ! [[ $REPLY =~ ^[Yy]$ ]]; then
	echo "${pass}"
	return 0
    fi
}

sudo_retry () {
    # Utility function to retry a command with sudo if it fails for lack of
    # permissions.
    local command="$@"
    local result=$(eval "${command}" 2>&1)
    if [[ "${result}" == *"Permission denied" ]]; then
	if [ "$AUTOCONFIRM" != true ]; then
	    read -p "Sudo required. Enter y to continue: " -n 1 -r
	    echo    # move to a new line
	    # TODO: replace with prompt
	    if ! [[ $REPLY =~ ^[Yy]$ ]]; then
		echo "Skipping"
		return 0
	    fi
	fi
	eval "sudo ${command}"
    fi
}

# ----------------------------------------------------------------------
# Pack and unpack

pack_module () {
    local module_name=$1
    local module_dir=$2
    local paths=$3

    # link paths
    declare -a linklinearray
    local OLDIFS=$IFS; local IFS=$'\n'
    for line in $paths; do
        IFS=$'\t' read filename dest <<< "${line}"
	local source="${module_dir}/src/${filename}"
	local target="${dest/#\~/$HOME}" # expand ~
	local linkline="${source};${target}"
	linklinearray+=( "${linkline}" )
    done
    local IFS=$OLDIFS
    linklines=$(join_by , "${linklinearray[@]}")

    output_module="${module_name}:${module_dir}:${linklines}"
    echo "${output_module}"
}

unpack_module () {
    local packed_module=$1
    local -n nameref=$2
    local -n dirref=$3
    local -n pathsref=$4

    IFS=':' read name dir paths <<< "${packed_module}"
    IFS="," read -a paths <<< ${paths//;/ }

    outref="{$name}"
    dirref="${dir}"
    pathsref="${paths}"
}

# ----------------------------------------------------------------------
# Parse module specifications

parse_module () {
    local module=$1
    local module_dir="${SCRIPT_DIR}/${module}"
    local module_file="${module_dir}/MODULE"
    if ! [ -f "${module_file}" ]; then
	>&2 echo "No 'MODULE file exists at ${module_file}"
	return
    fi
    source "${module_file}"
    if [ "$paths" ]; then
	output_module=$(pack_module $module $module_dir "${paths}")
	unset paths
    else
	>&2 echo "No paths defined for module '${module}'. Skipping."
	return
    fi
    echo "${output_module}"
}

parse_modules () {
    # Build module objects from config files
    for module in $MODULES; do
	MODULE_OBJS+=( $(parse_module "${module}") )
    done
}

# ----------------------------------------------------------------------
# Preview

check_existing () {
    # Check if a symlink exists
    local source=$1
    local target=$2
    if [ -L "${target}" ]; then
        if [ "$source" -ef "$target" ]; then
	    echo "correct"
	else
	    echo "wrong"
	fi
    elif [ -f "${target}" ] || [ -d "${target}" ]; then
	echo "occupied"
    else
	echo "empty"
    fi
}

preview_changes () {
    printf "Symlinks to create:\n\n"
    for module_obj in "${MODULE_OBJS[@]}"; do
	unpack_module "${module_obj}" name dir paths
	title "Module: ${name}"
	for pathset in "${paths[@]}"; do
	    read source dest <<< "${pathset}"
	    status=$(check_existing $source $dest)
	    color=${STATUS_COLORS[$status]}
	    printf "${color}$source\t->\t$dest\n${UNSET}"
	done
	echo
    done
}

# ----------------------------------------------------------------------
# Process

create_backup () {
    # Back up a file or directory at the target path to the backup directory in
    # the module directory.
    local target_path=$1
    local module_path=$2
    local timestamp=$(date +'%FT%H-%M-%S')
    local backup_dir="${module_path}/backups/${timestamp}/"
    mkdir -p $backup_dir
    sudo_retry mv $target_path $backup_dir
    BACKUPS_MADE+=( "${backup_dir}$(basename ${target_path})" )
}

clear_existing () {
    # Examine the target path of a symlink, and clear it if it exists, by
    # removing it if it is a symlink or by backing it up if it is a file.
    local target_path="${1/#\~/$HOME}"
    local module_path=$2
    if [ -L "${target_path}" ]; then
	sudo_retry rm $target_path
    elif [ -f "${target_path}" ] || [ -d "${target_path}" ]; then
	create_backup "${target_path}" "${module_path}"
    fi
}

create_link () {
    # Create a symlink from a source path in the dots repo to a target path in
    # the system.
    local source_path=$1
    local target_path=$2
    local module_path=$3
    if [ -f "${source_path}" ] || [ -d "${source_path}" ]; then
	sudo_retry ln -s "${source_path}" "${target_path}"
    else
        >&2 echo "No file or directory exists at ${source_path}"
    fi
}

process_paths () {
    local paths=$1
    local module_path=$2
    for line in "${paths[@]}"; do
        read source target <<< "${line}"
	clear_existing "${target}" "${module_path}"
	create_link "${source}" "${target}" "${module_path}"
    done
}

process_module () {
    local module_obj=$1
    unpack_module "${module_obj}" name dir paths
    process_paths "${paths}" "${dir}" # quote so not split in multiple args
}

process_modules () {
    for module in $MODULE_OBJS; do
	process_module "${module}"
    done
}

# ----------------------------------------------------------------------
# Main

USAGE="
Usage:
  ${0} [flags] [modules]

Flags:
  -h: Show usage
  -y: Autoconfirm
  -f <filepath>:  path to module file containing list of modules to install
  -d: show which actions will be performed without performing them.
"

while getopts ":ydf:h" opt; do
  case $opt in
    y)
      AUTOCONFIRM=true
      ;;
    f)
      MODULES_FILE=$OPTARG
      echo -e "Loading modules from the following file:\n${MODULES_FILE}\n"
      ;;
    d)
      DRYRUN=true
      ;;
    h)
      echo "${USAGE}"
      exit 0
      ;;
    \?)
      echo "Invalid option."
      echo "${USAGE}"
      exit 1
      ;;
  esac
done
shift $((OPTIND -1)) # shift past opts
ARG_MODULES="$@" # The rest of the arguments represent modules

main () {
    if [ "$MODULES_FILE" ]; then
	if [ "$ARG_MODULES" ]; then
	    >&2 echo "Can't specify modules as args while using module file."
	fi
	MODULES=$(cat "${MODULES_FILE}" | tr '\n' ' ')
    else
	MODULES="$ARG_MODULES"
    fi
    parse_modules
    preview_changes
    if [ "${DRYRUN}" != "true" ]; then
	if prompt "The above changes will be made" "Aborted"; then
	    process_modules
	    if [ "${BACKUPS_MADE[@]}" ]; then
		echo "Backups made:"
		for backup in "${BACKUPS_MADE}"; do
		    echo $backup
		done
	    fi
	fi
    fi
}

main
