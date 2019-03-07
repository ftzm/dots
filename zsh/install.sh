#!/usr/bin/env bash

ln -s ~/.dots/zsh/.zshrc ~/.zshrc

git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh-syntax-highlighting

# bashmarks
git clone git://github.com/huyng/bashmarks.git
mv ./bashmarks/bashmarks.sh ~/.bashmarks.sh
rm -r bashmarks

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
