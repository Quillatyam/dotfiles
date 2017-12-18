set -U EDITOR nvim

# Base16 Shell
#if status --is-interactive
#    eval sh $HOME/.config/base16-shell/scripts/base16-3024.sh
#end

# Paths etc.
set -x GOPATH $HOME/source/workspaces/go
set -x PATH $HOME/source/workspaces/go/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $HOME/.cabal/bin $PATH

# Aliasses.
alias e "$EDITOR"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux -2 new -A -s base"

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

#source local stuff.
source ~/.localrc
set -g fish_user_paths "/usr/local/bin" $fish_user_paths
