# Set universal editor per OS.
switch (uname)
    case Linux
        set -U EDITOR vim
        #use update-alternatives to make nvim default for vi and vim.
    case Darwin
        set -U EDITOR nvim
end

# Set PATH per OS.
switch (uname)
    case Linux
	set -U fish_user_paths /usr/local/go/bin $fish_user_paths
    case '*'
	set -x GOPATH $HOME/src/go
	set -U fish_user_paths $HOME/src/go/bin $fish_user_paths
end
	
# Aliasses.
alias e "$EDITOR"
alias gs "git status -sb"
alias gd "git diff"
alias gc "git commit"
alias gca "git commit -a"
alias gl "git pull --prune"
alias grb "git rebase -i @{u}"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux attach -t base ;or tmux new -s base"

