# Set universal editor.
set -U EDITOR vim

# Aliasses
alias e       "$EDITOR"
alias gs      "git status -sb"
alias gd      "git diff"
alias gc      "git commit"
alias gca     "git commit -a"
alias gl      "git pull --prune"
alias grb     "git rebase -i @{u}"
alias gac     "git add -A; git commit -m"
alias glog a  "git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"

# Load Xmodmap on linux (Ubuntu)
if [ (uname) == "Linux" ]
    xmodmap ~/.Xmodmap
end
	

