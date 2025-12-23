# User defined aliases
alias cp='cp -iv'                         # confirm before overwriting something
alias mv='mv -iv'
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias cryfs.mega='cryfs ~/MEGA/private.enc ~/Vaults/mega --unmount-idle 5'
alias cryfs.umega='cryfs-unmount ~/Vaults/mega'
alias veracrypt.wdd='veracrypt --mount /run/media/ivand/WDD/enc ~/Vaults/wdd'
alias veracrypt.uwdd='veracrypt --dismount'

alias open='xdg-open 2>/dev/null'
alias o='xdg-open 2>/dev/null'

alias k='kubectl'

alias delta='delta --light --line-numbers --side-by-side'

alias tree='tree --dirsfirst'
alias tr1='tree --dirsfirst -L 1 -a'
alias tr2='tree --dirsfirst -L 2 -a '
alias tr3='tree --dirsfirst -L 3 -a'

alias lzg='lazygit'

# used in kitty so open hyperlinks can work
alias ls='ls --hyperlink=auto --color=auto'

# open images in terminal (need ImageMagick)
alias img='kitty +kitten icat'
alias icat='kitty +kitten icat'

alias fuck='sudo $(fc -ln -1)'
