#!/bin/bash

echo "Hello master, $USER!"

chsh -s $(which -a zsh)

# Install base-devel yaourt (manjaro specific)

# Enable colors in /etc/pacman.conf

# check if curl is installed
# hash curl - no output ?!?
# sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install emacs
# setup prelude
# curl -L https://git.io/epre | sh

mkdir -pv ~/dev/src/
mkdir -pv ~/go/src/
mkdir -pv ~/go/pkg/
mkdir -pv ~/go/bin/

# TODO add interaction with user - u are going to import settings?

# checks for older settings - back up

# symo link to vim

# symo link emacs
$ln -s $HOME/dev/src/dotfiles/emacs/personal.el $HOME/.emacs.d/personal/personal.el

# symolink to zsh
ln -s ~/dev/src/dotfiles/zshrc.zsh ~/.zshrc

# git
ln -s $HOME/dev/src/dotfiles/git/user.gitconfig $HOME/.gitconfig

# ~/.profile
## set PATH so it includes user's private bin directories
## export GOPATH=$HOME/go
## export PATH="$GOPATH/bin:$HOME/bin:$HOME/.local/bin:$PATH"
