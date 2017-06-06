PHONY: setup-ubuntu save-install install

default: help

setup-ubuntu: ## Sets up Ubuntu and installs common things
	cd tag-ubuntu/install
install: ## Install the dotfiles using rcup.
	rcup -d ~/repositories/Quillatyam/dotfiles -x install -x install.sh -x README.md -x Makefile -t git -t vim -t fish -t tmux -t tmuxp
uninstall: ## Install the dotfiles using rcup.
	rcdn -d ~/repositories/Quillatyam/dotfiles -t git -t vim -t fish -t tmux -t tmuxp
help: ## This help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {sub("\\\\n",sprintf("\n%22c"," "), $$2);printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
