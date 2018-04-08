#!/bin/bash
#Filename: setup.sh

ln -s -i -v ~/.vim/vimrc   ~/.vimrc
ln -s -i -v ~/.vim/gvimrc  ~/.gvimrc
ln -s -i -v ~/.vim/bashrc  ~/.bashrc
ln -s -i -v ~/.vim/zshrc   ~/.zshrc
ln -s -i -v ~/.vim/ghci   ~/.ghci
ln -s -i -v ~/.vim/bash_aliases  ~/.bash_aliases
ln -s -i -v ~/.vim/gitconfig ~/.gitconfig

curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

vim +PlugInstall +qall

# Refer this to install powerline: http://askubuntu.com/questions/283908/how-can-i-install-and-use-powerline-plugin

