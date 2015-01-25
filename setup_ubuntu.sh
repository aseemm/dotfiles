#!/bin/bash
ln -s ~/.dotfiles/.bashrc ~/.bashrc
ln -s ~/.dotfiles/.bashrc_custom ~/.bashrc_custom
ln -s ~/.dotfiles/.emacs ~/.emacs
ln -s ~/.dotfiles/elisp ~/elisp
ln -s ~/.dotfiles/Xresources ~/Xresources

# aterm
sudo apt-get install aterm
sudo apt-get install -y rlwrap

# g++, etc
sudo apt-get install build-essential
sudo apt-get install ddd
sudo apt-get install gtkwave
sudo apt-get install tkcvs

# chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
sudo apt-get update
sudo apt-get install google-chrome-stable
### google-chrome

# python3
sudo apt-get install -y curl
sudo apt-get install -y python3

# heroku toolbelt
wget -qO- https://toolbelt.heroku.com/install-ubuntu.sh | sh

# keepass
   30  sudo apt-get install python-software-properties
   31  sudo apt-add-repository ppa:jtaylor/keepass
   32  sudo apt-get update
   33  sudo apt-get install keepass2
### keepass2

# restart
source ~/.bashrc