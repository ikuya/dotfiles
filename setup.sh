#!/bin/bash

DOTFILES=$(cd $(dirname $0); pwd)
EMACSD=$DOTFILES/.emacs.d
ZSH=$DOTFILES/zsh
TMUX=$DOTFILES/tmux
GIT=$DOTFILES/git
MISC=$DOTFILES/misc

EMACS_VER_ENABLED=24.4

M="mkdir"
T="touch"
L="ln"

deploy() {
    if [ $1 = $M ]; then
        if [ ! -e $2 ]; then
            mkdir -p $2
        else
            echo $2 'is already exists.'
        fi
    elif [ $1 = $T ]; then
        if [ ! -e $2 ]; then
            touch $2
        else
            echo $2 'is already exists.'
        fi
    elif [ $1 = $L ]; then
        if [ ! -e $3 ]; then
            ln -s $2 $3
        else
            echo $3 'is already exists.'
        fi
    fi
}

if [ -e /etc/debian_version ]; then
    read -p "Password? " pwd
    echo $pwd | sudo -S apt update
    echo $pwd | sudo -S apt upgrade
    sudo apt -y install tmux zsh emacs25 bc
fi

# Submodules initialization
cd $DOTFILES
git submodule update --init --recursive
cd $ZSH
git submodule update --init --recursive

# Emacs
which emacs > /dev/null
if [ $? = 0 ];then
    EMACS_VER=$(emacs --version | awk 'NR == 1 {print $3}' | cut -f1,2 -d.)
    if [ $(echo "$EMACS_VER >= $EMACS_VER_ENABLED" | bc) = 1 ]; then
        deploy $L $EMACSD $HOME/.emacs.d
        deploy $T $EMACSD/.scratch-log
        deploy $T $EMACSD/.scratch-log-prev
        deploy $M $EMACSD/backup
        cd $EMACSD
        # No need to call cask init. Cask file already exists.
        ./.cask/bin/cask
    else
        echo 'Required Emacs version ' $EMACS_VER_ENABLED ' or later.'
    fi
else
    echo 'Emacs is not installed.'
fi

# zsh
deploy $L $ZSH/.zshrc $HOME/.zshrc
deploy $M $HOME/.cache/shell
deploy $L $ZSH/zaw $HOME/.zaw

# Tmux
deploy $L $TMUX/.tmux.conf $HOME/.tmux.conf

# Git
deploy $L $GIT/.gitconfig $HOME/.gitconfig

# bin
deploy $M $HOME/bin
deploy $L $TMUX/loadaverage.sh $HOME/bin/loadaverage
deploy $L $TMUX/used_mem.sh $HOME/bin/used_mem
if [[ ${OSTYPE} =~ darwin* ]]; then
    deploy $L $MISC/mem.sh $HOME/bin/mem
fi

## Vi
deploy $L $DOTFILES/.vimrc $HOME/.vimrc
deploy $M $HOME/.vim_swp

## Misc.
deploy $L $MISC/.inputrc $HOME/.inputrc
deploy $L $MISC/.screenrc $HOME/.screenrc

echo 'Setup.sh successfully finished.'
exit 0
