# dotfiles
My dotfiles and scripts

# How to install
First backup any config files you have!

For convenience clone the repo into `~/.dotfiles` directory.

Then you may install one of the preferred config via

```
stow <package>  # just for single package
stow            # for all packages
```

As <package> may be any top level folder in the project.

## Prerequisites
Some packages needs additional dependencies to work properly. Below are listed only packages that need such dependencies.

### zsh
`oh-my-zsh`
`zoxide`
`kubectl`
`fzf`


### kitty
`zsh`
`fish` # depending on the primary shell

### nvim
`go`

# Notable projects
[dotfiles.github]( https://dotfiles.github.io/ ) </br>
[dotfiles.stow]( https://gitlab.com/waterkip/dotty )
