set -u EDITOR "vim"

# Base16 Shell
#if status --is-interactive
#    eval sh $HOME/.config/base16-shell/scripts/base16-3024.sh
#end

# Paths etc.
set -x PATH $HOME/.local/bin $PATH
set -x PATH $HOME/.cabal/bin $PATH
set -x PATH $HOME/Library/Python/3.6/bin $PATH
set -x PATH $HOME/.cargo/bin $PATH
set -x PATH $HOME/source/go/bin $PATH
set -x GOPATH $HOME/source/go

# HLA
set -x PATH $HOME/Applications/hla $PATH
set -gx hlalib $HOME/Applications/hla/hlalib
set -gx hlainc $HOME/Applications/hla/include
alias hla='hla -main:_main -l"macosx_version_min 10.9" -l"lSystem" -l"no_pie"'

# Aliasses.
alias e "$EDITOR"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux -2 new -A -s base"
alias nvimdiff "nvim -d"
alias t "todo.sh"

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
