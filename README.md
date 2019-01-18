My Dotfiles
--------
Mostly aimed at Haskell, Rust and Typescript.


## RCM
RCM manages my dotfiles, see [https://github.com/thoughtbot/rcm](https://github.com/thoughtbot/rcm) for
more information.

## Install RCM
**macOS:** 
```
brew install rcm
```

**Linux:**
```
sudo add-apt-repository ppa:martin-frost/thoughtbot-rcm
sudo apt-get update
sudo apt-get install rcm
```
**FreeBSD**
```
pkg install rcm
```

## Install dotfiles
To install all dotfiles run `rcup`, or install per tag by running `rcup -t <tag> -v`.

## Install Doom Emacs
```
# clone doom
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d

# checkout the development branch
cd .emacsd && git checkout develop && git pull

# install doom
.emacsd/bin/doom install

# install personal layer/config.
rcup -t emacs -v
```
