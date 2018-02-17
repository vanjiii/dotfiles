#!/bin/bash

echo "Hello master, $USER!"

chsh -s $(which -a zsh)

# TODO add interaction with user - u are going to import settings?

# checks for older settings - back up

# symo link to vim

# symo link emacs
$ln -s $HOME/dev/src/dotfiles/emacs/personal.el $HOME/.emacs.d/personal.personal.el

# symolink to zsh
ln -s ~/dev/src/dotfiles/zshrc.zsh ~/.zshrc

# git
ln -s $HOME/dev/src/dotfiles/git/user.gitconfig $HOME/.gitconfig

# ~/.profile
## set PATH so it includes user's private bin directories
## export GOPATH=$HOME/go
## export PATH="$GOPATH/bin:$HOME/bin:$HOME/.local/bin:$PATH"
