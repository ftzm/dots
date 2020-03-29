{pkgs, ...}:

pkgs.writeShellScriptBin "get-auth" ''
set -eu

authinfo=~/.authinfo
[ -f $authinfo ] || (echo "''${authinfo} is missing"; exit 1)

function getVar() {
    local names="''${@:2}"
    regex="(''${names// /|}) ([[:alnum:][:punct:]]*)"
    [[ $names && $1 =~ $regex ]] && echo "''${BASH_REMATCH[2]}"
}

while read line; do
    machine=$(getVar "''${line}" "machine" "host")
    login=$(getVar "''${line}" "login" "name")
    [ $machine == $1 ] &&
    [ $login == $2 ] &&
    getVar "''${line}" "password" &&
    exit 0
done <$authinfo
exit 1
''
