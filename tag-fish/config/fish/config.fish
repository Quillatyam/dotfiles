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

set -x LANG "en_US.UTF-8"
set -x LANGUAGE "en_US.UTF-8"
set -x LC_CTYPE "en_US.UTF-8"
set -x LC_NUMERIC "en_US.UTF-8"
set -x LC_TIME "en_US.UTF-8"
set -x LC_COLLATE "en_US.UTF-8"
set -x LC_MONETARY "en_US.UTF-8"
set -x LC_MESSAGES "en_US.UTF-8"
set -x LC_PAPER "en_US.UTF-8"
set -x LC_NAME "en_US.UTF-8"
set -x LC_ADDRESS "en_US.UTF-8"
set -x LC_TELEPHONE "en_US.UTF-8"
set -x LC_MEASUREMENT "en_US.UTF-8"
set -x LC_IDENTIFICATION "en_US.UTF-8"
set -x LC_ALL "en_US.UTF-8"

