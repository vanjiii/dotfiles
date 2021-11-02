bootstrap-vim: ## Bootstrap vim configs
	mkdir -p $(HOME)/.config/nvim
	@echo "source $(PWD)/vim/init.vim" > $(HOME)/.config/nvim/init.vim

bootstrap-git: ## Bootstrap global git configs and some scripts
	ln -sf $(PWD)/git/user.gitconfig $(HOME)/.gitconfig
	mkdir -p $(HOME)/dev/bin
	ln -fs $(PWD)/git/new-branch $(HOME)/dev/bin/git-new-branch
	chmod +x $(HOME)/dev/bin/git-new-branch

bootstrap-cheat: ## Bootstrap cheat config file
	mkdir -p $(HOME)/.config/cheat
	ln -sf $(PWD)/cheat/conf.yaml $(HOME)/.config/cheat/conf.yml

bootstrap-zsh: ## Bootstrap zsh config files
	ln -sf $(PWD)/zsh/zshenv.zsh $(HOME)/.zshenv
	ln -sf $(PWD)/zsh/zshrc.zsh $(HOME)/.zshrc

help:  ## Display this help screen
	@grep -h -E '^[\.a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
