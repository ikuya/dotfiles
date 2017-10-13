#!/bin/sh

DOTFILE_DIR=$(cd $(dirname $0); pwd)
cd $DOTFILE_DIR

git submodule update --init --recursive

mkdir $HOME/bin

## Symbolic link
ln -s $DOTFILE_DIR/.emacs.d $HOME/.emacs.d
ln -s $DOTFILE_DIR/zsh/.zshrc $HOME/.zshrc
ln -s $DOTFILE_DIR/git/.gitconfig $HOME/.gitconfig
ln -s $DOTFILE_DIR/tmux/.tmux.conf $HOME/.tmux.conf
ln -s $DOTFILE_DIR/tmux/loadaverage.sh $HOME/bin/loadaverage.sh
ln -s $DOTFILE_DIR/tmux/used_mem.sh $HOME/bin/used_mem.sh
ln -s $DOTFILE_DIR/misc/.inputrc $HOME/.inputrc
ln -s $DOTFILE_DIR/misc/.screenrc $HOME/.screenrc

## Emacs
touch $DOTFILE_DIR/.emacs.d/.scratch-log-prev
touch $DOTFILE_DIR/.emacs.d/.scratch-log
cd $DOTFILE_DIR/.emacs.d/elisp/helm
EMACSLOADPATH="$DOTFILE_DIR/.emacs.d/elisp/emacs-async:"
make
