# TODO: move those `eval` functions to happen once per login
# then loaded
# aka introduce caching for those

command -v zoxide &> /dev/null && eval "$(zoxide init zsh)"
command -v kubectl &> /dev/null && source <(kubectl completion zsh)

command -v gup &> /dev/null && eval "$(gup completion zsh)"

command -v fzf &> /dev/null && source <(fzf --zsh)

command -v starship &> /dev/null && eval "$(starship init zsh)"

if [ -e /home/ivand/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ivand/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
