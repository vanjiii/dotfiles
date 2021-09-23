# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

typeset -U PATH path

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

GOPATH=$HOME/go
export GOPATH

export PATH="$GOPATH/bin:$PATH"

GOBIN=$GOPATH/bin
export GOBIN

# openvpn
export PATH=$(brew --prefix openvpn)/sbin:$PATH

export KUBECONFIG=$GOPATH/src/github.com/gardener/gardener/hack/local-development/local-garden/kubeconfigs/default-admin.conf

export PATH="${PATH}:${HOME}/.krew/bin"

# Options to fzf command
export FZF_COMPLETION_OPTS='--border --info=inline --cycle --preview="head -$LINES {}"'

# Shit to make work GPG again under MacOS.
export GPG_TTY=$(tty)
