#!/bin/sh

DOTFILES=$(cd $(dirname $0); pwd)
EMACSD=$DOTFILES/.emacs.d
ZSH=$DOTFILES/zsh
TMUX=$DOTFILES/tmux
GIT=$DOTFILES/git
MISC=$DOTFILES/misc

EMACS_VER_ENABLED=24.4

deploy() {
    if [ ! -e $2 ]; then
        case $1 in
            "m") mkdir $2 ;;
            "t") touch $2 ;;
            "l") ln -s $3 $2 ;;
        esac
    fi
}

# Submodules initialization
cd $DOTFILES
git submodule update --init --recursive

# Emacs
which emacs > /dev/null
if [ $? = 0 ];then
    EMACS_VER=$(emacs --version | awk 'NR == 1 {print $3}' | cut -f1,2 -d.)
    if [ $(echo "$EMACS_VER >= $EMACS_VER_ENABLED" | bc) = 1 ]; then
        deploy 'l' $HOME/.emacs.d $EMACSD
        deploy 't' $EMACSD/.scratch-log
        deploy 't' $EMACSD/.scratch-log-prev
        ## Cask
        ### Caskファイルはすでにあるので、cask init は不要
        cd $EMACSD
        ./.cask/bin/cask
    fi
fi

# zsh
deploy 'l' $HOME/.zshrc $ZSH/.zshrc
deploy 't' $HOME/.zlogin
echo "export PATH=$EMACSD/.cask/bin:$PATH" >> $HOME/.zlogin

# Tmux
deploy 'l' $HOME/.tmux.conf $TMUX/.tmux.conf

# Git
deploy 'l' $HOME/.gitconfig $GIT/.gitconfig

# bin
deploy 'm' $HOME/bin
deploy 'l' $HOME/bin/loadaverage.sh $TMUX/loadaverage.sh
deploy 'l' $HOME/bin/used_mem.sh $TMUX/used_mem.sh

## Misc.
deploy 'l' $HOME/.inputrc $MISC/.inputrc
deploy 'l' $HOME/.screenrc $MISC/.screenrc
