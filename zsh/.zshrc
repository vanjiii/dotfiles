# slow git repos
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Duplicate rows
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Disable the Xon/Xoff so the C-s to work and be able to cycle forward.
stty -ixon

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
)

source $ZSH/oh-my-zsh.sh

# User defined aliases
alias cp='cp -iv'                         # confirm before overwriting something
alias mv='mv -iv'
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias cryfs.mega='cryfs ~/MEGA/private.enc ~/Vaults/mega --unmount-idle 5'
alias cryfs.umega='cryfs-unmount "/home/vnj/Vaults/mega"'

alias open='xdg-open 2>/dev/null'
alias o='xdg-open 2>/dev/null'

alias k='kubectl'

alias delta='delta --light --line-numbers --side-by-side'

alias tree='tree --dirsfirst'
alias tr1='tree --dirsfirst -L 1 -a'
alias tr2='tree --dirsfirst -L 2 -a '
alias tr3='tree --dirsfirst -L 3 -a'

alias lz='lazygit'

# used in kitty so open hyperlinks can work
alias ls='ls --hyperlink=auto --color=auto'

# open images in terminal (need ImageMagick)
alias img='kitty +kitten icat'
alias icat='kitty +kitten icat'

alias speedtest=speedtest-cli

alias fuck='sudo $(fc -ln -1)'

alias bwu='export BW_SESSION="$(bw unlock --raw)"'

# fzf history - repeat history
fhistory() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# ex - archive extractor
# usage: ex <file>
ex ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1   ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# pw - a random password generator
#
# if not specify it will generate 32 char long password
# usage: pw [Optional N]
pw ()
{
    length=32
    if [ "$1" != "" ]
    then
        length=$1
    fi
    </dev/urandom tr -dc '12345!@#$%qwertQWERTasdfgASDFGzxcvbZXCVB' | head -c$length; echo ""
}

eval "$(zoxide init zsh)"
source <(kubectl completion zsh)
