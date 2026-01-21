# Completion configuration

# Advanced fuzzy matching
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only

# Fuzzy matching with multiple attempts
zstyle ':completion:*' matcher-list \
  '' \
  'm:{a-zA-Z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

# Group matches and describe
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}-- no matches found --%f'

# Menu selection
zstyle ':completion:*' menu select

# Colors in completion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Cache completion
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

zstyle ':completion:*:*:git:*' script $ZSHCOMPLETIONS/git.zsh
