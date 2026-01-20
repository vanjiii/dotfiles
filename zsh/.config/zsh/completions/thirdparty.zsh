# TODO: move those `eval` functions to happen once per login
# then loaded
# aka introduce caching for those
eval "$(zoxide init zsh)"
source <(kubectl completion zsh)

command -v gup &> /dev/null && eval "$(gup completion zsh)"

source <(fzf --zsh)

eval "$(starship init zsh)"

if [ -e /home/ivand/.nix-profile/etc/profile.d/nix.sh ]; then . /home/ivand/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# bun completions
[ -s "/home/ivand/.bun/_bun" ] && source "/home/ivand/.bun/_bun"
