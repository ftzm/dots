# Aliases
alias vim=nvim
alias k=kubectl
alias d=docker

fpath=(~/.zsh/completions $fpath)

/usr/bin/setxkbmap -option "caps:swapescape"

export TERM="xterm-256color"
export PANEL_FIFO="/tmp/panel-fifo"
export PANEL_FIFO
export EDITOR="nvim"
export ANDROID_HOME=/opt/android-sdk
# export FZF_DEFAULT_OPTS='
# --color 16,hl:4,fg+:136,hl+:9,bg+:8,info:37'
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS='
--color fg:223,hl:166,fg+:235,bg+:72,hl+:229
--color info:246,prompt:223,spinner:142,pointer:166,marker:166
'

autoload -Uz promptinit
promptinit
prompt adam1

#highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#highlight completion
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
autoload -Uz compinit
compinit

#make mpc use mpd on debserv
export MPD_HOST=192.168.0.102

# User configuration

export PATH="$PATH:/home/matt/.local/bin"
export PATH="$PATH:/home/matt/bin"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

#history
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.

alias ls='ls --color=auto --group-directories-first'

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "$key[Up]"   ]] && bindkey -- "$key[Up]"   up-line-or-beginning-search
[[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search


DEFAULT_USER="matt"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#use vim input method
export KEYTIMEOUT=1
bindkey -v

# aws completion
source ~/.local/bin/aws_zsh_completer.sh

