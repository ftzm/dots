# init direnv
eval "$(direnv hook zsh)"

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/matt/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

plugins=(git vi-mode docker)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"



# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
#if [[ -n $SSH_CONNECTION ]]; then
#  export EDITOR='vim'
#else
#  export EDITOR='nvim'
#fi

export EDITOR='e'

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"


# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias vim=nvim
alias k=kubectl

# Local path stuff
export PATH="$PATH:/home/matt/.local/bin"
export PATH="$PATH:/home/matt/bin"

#fzf
export HISTCONTROL=ignoreboth:erasedups
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS='
--color fg:223,hl:166,fg+:235,bg+:4,hl+:223
--color info:246,prompt:223,spinner:142,pointer:166,marker:166
'

# Ask for passwords in cli
unset SSH_ASKPASS
unset GIT_ASKPASS

#highlighting
source ~/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# bashmarks
# unalias conflicting aliases for bashmarks
unalias l
unalias g
source ~/.bashmarks.sh
