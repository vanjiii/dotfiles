typeset -U PATH path

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

GOPATH=$HOME/go
export GOPATH

export PATH="$GOPATH/bin:$PATH"

GOBIN=$GOPATH/bin
export GOBIN

export LESS='-F -r'
