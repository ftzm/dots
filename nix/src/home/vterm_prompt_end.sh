# For emacs-libvterm and ZSH.
# Source: https://github.com/akermu/emacs-libvterm
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
