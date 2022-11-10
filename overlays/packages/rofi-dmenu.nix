{pkgs, ...}:

pkgs.writeShellScriptBin "rofi-dmenu" ''
${pkgs.rofi-wayland}/bin/rofi \
	-show run \
	-location 1 \
	-width 100 \
	-theme-str '#window { border: 0 0 1px 0; children: [ horibox ]; anchor: north; location: north;}' \
	-theme-str '#listview { spacing: 5px; layout: horizontal; }' \
	-theme-str '#horibox {orientation: horizontal; children:   [ prompt, entry, listview ];}' \
	-theme-str '#entry {expand:     false; width:      10em;}' \
	-sorting-method fzf \
	-sort \
  -history \
	-theme-str '#prompt { enabled: false; }'
''
