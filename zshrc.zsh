# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Disable the Xon/Xoff so the C-s to work and be able to cycle forward.
stty -ixon

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    archlinux
    golang
)

source $ZSH/oh-my-zsh.sh

# User configuration

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# User defined aliases
alias et='emacsclient -t'
alias ec='emacsclient -c'
alias e='emacsclient -t'

alias gl=git log --oneline --all --graph --decorate  $*

alias cp='cp -iv'                         # confirm before overwriting something
alias mv='mv -iv'
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias l='pwd && ls -lah --color=tty --group-directories-first'

alias gb='git --no-pager branch'

alias kubectl.stage='kubectl --kubeconfig=$HOME/dev/src/scripts/finergodom-cluster-kubeconfig.yaml'
alias cryfs.mega='cryfs ~/MEGA/private.enc ~/Vaults/MegaCloud --unmount-idle 5'
alias cryfs.umega='cryfs-unmount "/home/vanjiii/Vaults/MegaCloud"'


update-go-tools(){
    go get -u -v github.com/mdempsky/gocode &&
    go get -u -v github.com/rogpeppe/godef &&
    go get -u -v golang.org/x/tools/cmd/gorename &&
    go get -u -v golang.org/x/tools/cmd/guru &&
    go get -u -v golang.org/x/tools/cmd/goimports &&
    go get -u -v golang.org/x/tools/gopls &&
    rm -f /tmp/gocode-daemon.vanjiii &&
    gocode close
}

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
            *.rar)       unrar x $1     ;;
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

RPROMPT=%*
# Useful CMDs
#
# --> sed  -i "s/auth.TWithRole/user.TWithRole/" `find ./invoices -name "*.go"`
# `-i` in_place or apply it on the files. Nit: use without `-i` to see the changes.
# "s/regex_to_find/string_for_subst/"
# **/*.go == `find . -name "*.go"`
