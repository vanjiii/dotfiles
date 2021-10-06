bootstrap-vim: ## Bootstrap vim configs
	@echo "source $(PWD)/vim/init.vim" > $(HOME)/.config/nvim/init.vim

help:  ## Display this help screen
	@grep -h -E '^[\.a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
