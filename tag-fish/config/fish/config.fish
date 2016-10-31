# Set universal editor.
set -U EDITOR vim

# Go lib and GOPATH.
set -U fish_user_paths /usr/local/go/bin $fish_user_paths
set -x GOPATH $HOME/src/go

# Aliasses
alias e "$EDITOR"
alias gs "git status -sb"
alias gd "git diff"
alias gc "git commit"
alias gca "git commit -a"
alias gl "git pull --prune"
alias grb "git rebase -i @{u}"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux attach -t base ;or tmux new -s base"

# Load Xmodmap on linux (Ubuntu)
if [ (uname) == "Linux" ]
    xmodmap ~/.Xmodmap
end

if [ ! $TERM =~ screen ]
    exec tmux
end


	

