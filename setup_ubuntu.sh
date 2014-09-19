#!/bin/bash
ln -s ~/.dotfiles/.bashrc ~/.bashrc
ln -s ~/.dotfiles/.bashrc_custom ~/.bashrc_custom
ln -s ~/.dotfiles/.emacs ~/.emacs
ln -s ~/.dotfiles/elisp ~/elisp
ln -s ~/.dotfiles/Xresources ~/Xresources

# aterm
sudo apt-get install aterm

# g++, etc
sudo apt-get install build-essential
sudo apt-get install ddd
sudo apt-get install gtkwave

# chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
sudo apt-get update
sudo apt-get install google-chrome-stable
