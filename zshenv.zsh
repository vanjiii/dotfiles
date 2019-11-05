typeset -U PATH path

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

GOPATH=$HOME/go
export GOPATH

SUNSHINE_ENV=dev
export SUNSHINE_ENV

export PATH="$GOPATH/bin:$PATH"

export LESS='-F -r'
