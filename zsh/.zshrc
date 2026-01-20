# vim:syntax=zsh
# vim:filetype=zsh
#
# Functionality

# TODO:
# - implement healthcheck - it will check if depenencies are present (like dup)

export ZSHCONFIG=$HOME/.config/zsh
export ZSHPROFILE=$ZSHCONFIG/profiles
export ZSHCOMPLETIONS=$ZSHCONFIG/completions

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

setopt autocd                   # cd by typing directory name
setopt interactive_comments     # allow comments in interactive shell
setopt hist_ignore_all_dups     # remove older duplicate entries from history
setopt hist_reduce_blanks       # remove superfluous blanks from history
setopt inc_append_history       # append to history immediately
setopt share_history            # share history between sessions

# Key bindings
bindkey -e  # Emacs-style (or -v for vim-style)
bindkey '^R' history-incremental-search-backward


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

[[ -f "$ZSHPROFILE/work.zsh" ]] && source "$ZSHPROFILE/work.zsh"

. $ZSHCOMPLETIONS/builtin.zsh
. $ZSHCOMPLETIONS/thirdparty.zsh

. $ZSHCONFIG/title.zsh

. $ZSHCONFIG/aliases.zsh
