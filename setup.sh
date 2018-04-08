#!/bin/bash
#Filename: setup.sh

ln -s -i -v ~/.vim/vimrc   ~/.vimrc
ln -s -i -v ~/.vim/gvimrc  ~/.gvimrc
ln -s -i -v ~/.vim/bashrc  ~/.bashrc
ln -s -i -v ~/.vim/zshrc   ~/.zshrc
ln -s -i -v ~/.vim/ghci   ~/.ghci
ln -s -i -v ~/.vim/bash_aliases  ~/.bash_aliases
ln -s -i -v ~/.vim/gitconfig ~/.gitconfig
ln -s -i -v ~/.vim/hgrc   ~/.hgrc
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Refer this to install powerline: http://askubuntu.com/questions/283908/how-can-i-install-and-use-powerline-plugin

