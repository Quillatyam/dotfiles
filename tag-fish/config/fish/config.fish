set -x EDITOR "vim"

# Paths etc.
#set -gx fish_user_paths $HOME/.local/bin $fish_user_paths
#set -gx fish_user_paths $HOME/.cabal/bin $fish_user_paths
#set -gx fish_user_paths $HOME/Library/Python/3.6/bin $fish_user_paths
set -gx fish_user_paths $HOME/.cargo/bin $fish_user_paths
set -gx fish_user_paths $HOME/source/go/bin $fish_user_paths
#set -gx fish_user_paths $HOME/.composer/vendor/bin $fish_user_paths
set -gx fish_user_paths "/usr/local/bin" $fish_user_paths

# Golang
set -x GOPATH $HOME/source/go

# HLA
set -gx fish_user_paths $HOME/Applications/hla $fish_user_paths
set -gx hlalib $HOME/Applications/hla/hlalib
set -gx hlainc $HOME/Applications/hla/include
alias hla='hla -main:_main -l"macosx_version_min 10.9" -l"lSystem" -l"no_pie"'

# Aliasses.
alias e "$EDITOR"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux -2 new -A -s base"
alias vd "vimdiff"

# Emacs aliasses
alias emd  '/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em  '/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
alias emn  '/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c --no-wait'

# Git aliasses
alias gs "git status -sb"
alias gd "git diff"
alias gc "git commit"
alias gca "git commit -a"
alias gl "git pull --prune"
alias grb "git rebase -i @{u}"

# Darcs aliasses
alias dw "darcs whatsnew"
alias ds "darcs whatsnew -s"
alias ddiff "darcs diff"
alias dl "darcs log"
alias da "darcs add"
alias dr "darcs record"
set -U DARCS_EDITOR vim

# GPG 
#gpgconf --launch gpg-agent
# Ensure that GPG Agent is used as the SSH agent
set -e SSH_AUTH_SOCK
set -U -x SSH_AUTH_SOCK ~/.gnupg/S.gpg-agent.ssh

# PHP
set -g fish_user_paths "/usr/local/opt/php@7.1/bin" $fish_user_paths

# Source local stuff.
source ~/.localrc

# Source Nix last
source ~/.config/fish/nix-daemon.fish
