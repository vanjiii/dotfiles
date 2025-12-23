# vim:syntax=zsh
# vim:filetype=zsh
#
# Functionality
#

export ZSHCONFIG=$HOME/.config/zsh
export ZSHPROFILE=$ZSHCONFIG/profiles

autoload -Uz compinit
# Only rebuild cache once a day (performance optimization)
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

# slow git repos
DISABLE_UNTRACKED_FILES_DIRTY="true"

export LANG=en_US.UTF-8

# Disable the Xon/Xoff so the C-s to work and be able to cycle forward.
stty -ixon


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

# TODO: move those `eval` functions to happen once per login
# then loaded
# aka introduce caching for those
eval "$(zoxide init zsh)"
source <(kubectl completion zsh)

# bun completions
[ -s "/home/ivand/.bun/_bun" ] && source "/home/ivand/.bun/_bun"

source <(fzf --zsh)

eval "$(starship init zsh)"

[[ -f "$HOME/.zsh/profiles/work.zsh" ]] && source "$ZSHPROFILE/work.zsh"

if [ -e /home/ivand/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ivand/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

. $ZSHCONFIG/title.zsh

. $ZSHCONFIG/aliases.zsh
