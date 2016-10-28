# Set universal editor.
set -U EDITOR vim

# Aliasses
alias e "$EDITOR"
alias gs "git status -sb"
alias gd "git diff"
alias gc "git commit"
alias gca "git commit -a"
alias gl "git pull --prune"
alias grb "git rebase -i @{u}"

# Load Xmodmap on linux (Ubuntu)
if [ (uname) == "Linux" ]
    xmodmap ~/.Xmodmap
end
	

