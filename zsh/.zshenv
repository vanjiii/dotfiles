#
# Environment variables
#

export GOBIN=$HOME/go/bin

export BUN_INSTALL="$HOME/.bun"

export VISUAL=vi

export PAGER=less
export LESS='-F -S -R -M -i'

export LANG=en_US.UTF-8

export HISTFILE=~/.zsh_history
export SAVEHIST=10000
export HISTSIZE=10000

# Sets bat theme.
# Also used in fzf.vim preview window (for better colorscheme).
export BAT_THEME="GitHub"

typeset -U PATH path

path=(
    $HOME/.local/bin
    /usr/local/{bin,sbin}
    $GOBIN
    $BUN_INSTALL/bin
    $HOME/.npm-global/bin
    $path
)
