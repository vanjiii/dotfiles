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
    pass
    docker
)

source $ZSH/oh-my-zsh.sh

# User defined aliases
alias gl=git log --oneline --all --graph --decorate  $*

alias cp='cp -iv'                         # confirm before overwriting something
alias mv='mv -iv'
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias cryfs.mega='cryfs ~/MEGA/private.enc ~/Vaults/MegaCloud --unmount-idle 5'
alias cryfs.umega='cryfs-unmount "/home/vanjiii/Vaults/MegaCloud"'

alias open='xdg-open 2>/dev/null'
alias o='xdg-open 2>/dev/null'

alias xcopy='xclip -selection clipboard'
alias xpaste='xclip -selection clipboard -o'

alias k='kubectl'
alias kp='kubectl -n payments-self-service'

alias delta='delta --light --line-numbers --side-by-side'

alias e='dolphin . &'
alias tree='tree --dirsfirst'

alias lz='lazygit'

alias magic='automatic-receipts'

#
# # ex - archive extractor
# # usage: ex <file>
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

# pwrand - a random password generator
#
# if not specify it will generate 32 char long password
# usage: randpw [Optional N]
pwrand ()
{
    length=32
    if [ "$1" != "" ]
    then
        length=$1
    fi
    </dev/urandom tr -dc '12345!@#$%qwertQWERTasdfgASDFGzxcvbZXCVB' | head -c$length; echo ""
}

eval "$(zoxide init zsh)"

