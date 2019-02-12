#!/usr/bin/env bash

ln -s ~/.dots/zsh/.zshrc ~/.zshrc
git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh-syntax-highlighting

git clone git://github.com/huyng/bashmarks.git
mv ./bashmarks/bashmarks.sh ~/.bashmarks.sh
rm -r bashmarks

