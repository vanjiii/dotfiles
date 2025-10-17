# This (due to mirroring) also keeps the entries in PATH unique.
typeset -U PATH path

VISUAL=vi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/.bin:/usr/local/bin:$PATH

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

GOBIN=$HOME/go/bin
export GOBIN
export PATH=$GOBIN:$PATH

export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

# kafka shit
export PATH=$HOME/.local/bin/kafka/bin:$PATH

# npm
export PATH=$HOME/.npm-global/bin:$PATH

# Shit to make work GPG again under MacOS.
export GPG_TTY=$(tty)

# Sets bat theme.
# Also used in fzf.vim preview window (for better colorscheme).
export BAT_THEME="GitHub"

export PAGER='less -F -S -R -M -i'

[[ -f "$HOME/.zsh/profiles/work.zsh" ]] && source  "$HOME/.zsh/profiles/work.zsh"
