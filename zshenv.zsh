typeset -U PATH path

HISTFILE=~/.zsh_history
SAVEHIST=10000
HISTSIZE=10000

GOPATH=$HOME/go
export GOPATH

SUNSHINE_ENV=dev
export SUNSHINE_ENV

export PATH="$GOPATH/bin:$PATH"

GOBIN=$GOPATH/bin
export GOBIN

export LESS='-F -r'

GTK_USE_PORTAL=1
export GTK_USE_PORTAL

export TERM=xterm-256color
