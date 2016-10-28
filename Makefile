PHONY: setup-ubuntu save-install install

default: binary

setup-ubuntu:
	cd tag-ubuntu/install
save-install:
	rcup -x install -x install.sh -x README.md -x Makefile -t git -t ubuntu -t vim -t fish -g > install.sh 
install:
	./install.sh
help: ## this help
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {sub("\\\\n",sprintf("\n%22c"," "), $$2);printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
