PHONY: setup-ubuntu save-install install

default: help

setup-ubuntu: ## Sets up Ubuntu and installs common things
	cd tag-ubuntu/install
install: ## Install the dotfiles using rcup.
	rcup -x install -x install.sh -x README.md -x Makefile -t git -t ubuntu -t vim -t fish -t tmux
install-vim: ## Install vim dotfiles
	rcup -x install -x install.sh -x README.md -x Makefile -t vim
install-tmux: ## Install vim dotfiles
	rcup -x install -x install.sh -x README.md -x Makefile -t tmux
install-fish: ## Install fish dotfiles
	rcup -x install -x install.sh -x README.md -x Makefile -t fish
help: ## This help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {sub("\\\\n",sprintf("\n%22c"," "), $$2);printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
