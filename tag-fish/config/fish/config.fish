set -x EDITOR "vim"

# Paths etc.
set -gx fish_user_paths $HOME/.local/bin $fish_user_paths
set -gx fish_user_paths $HOME/.cabal/bin $fish_user_paths
set -gx fish_user_paths $HOME/source/go/bin $fish_user_paths
set -gx fish_user_paths $HOME/.composer/vendor/bin $fish_user_paths
set -gx fish_user_paths $HOME/.emacs.d/bin $fish_user_paths
set -gx fish_user_paths "/usr/local/bin" $fish_user_paths

# Rust
set -gx fish_user_paths $HOME/.cargo/bin $fish_user_paths
set -gx RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src

# Golang
set -x GOPATH $HOME/source/go

# HLA
#set -gx fish_user_paths $HOME/Applications/hla $fish_user_paths
#set -gx hlalib $HOME/Applications/hla/hlalib
#set -gx hlainc $HOME/Applications/hla/include
#alias hla='hla -main:_main -l"macosx_version_min 10.9" -l"lSystem" -l"no_pie"'

# Aliasses.
#alias e "$EDITOR"
alias passgen "pass generate --no-symbols --clip"
alias tm "tmux -2 new -A -s base"
alias vd "vimdiff"

# Emacs aliasses

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

# Exports
set -x LANG "en_US.UTF-8"
set -x LC_ALL "en_US.UTF-8"
set -x MANPAGER "less -X"
set -x GIT_COMPLETION_CHECKOUT_NO_GUESS "1"
set -x MAILCHECK "0"

# GNUPG
gpg-agent --daemon --enable-ssh-support > /dev/null 2>&1
set -x GPG_TTY (tty)
gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)

# Base16 Shell
if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
    base16-3024
end

# Source local stuff.
source ~/.localrc
