{pkgs, ...}:

pkgs.writeShellScriptBin "rofi-filer" ''
search_dirs="./Downloads ./Desktop ./dev"

file_path="$(rg --no-messages --files $search_dirs \
     -g "!{*.srt,*.rar,*.txt,*.zip,*.nfo,dist*}" \
     --max-depth=4 | ${pkgs.rofi}/bin/rofi -threads 0 \
     -width 100 \
     -location 1 \
     -theme-str "#window { border: 0 0 1 0;}" \
     -dmenu -sort -sorting-method fzf -i -p "find")"

if [ "$file_path" ]; then
    ${pkgs.xdg_utils}/bin/xdg-open "$file_path"
fi
''
