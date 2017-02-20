# Set universal editor per OS.
switch (uname)
    case Linux
        set -U EDITOR vim
        #use update-alternatives to make nvim default for vi and vim.
    case Darwin
        set -U EDITOR nvim
end

# Paths etc.
set -x GOPATH $HOME/src/go
set -x PATH $HOME/src/go/bin $PATH
set -x PATH $HOME/.local/bin $PATH
set -x PATH $HOME/.cabal/bin $PATH
set -x PATH $HOME/.stack/programs/x86_64-osx/ghc-8.0.1/bin $PATH
set -x PATH $HOME/.composer/vendor/bin $PATH

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

#source local stuff.
source ~/.localrc
