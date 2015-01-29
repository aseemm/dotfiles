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

# calibre
sudo -v && wget -nv -O- https://raw.githubusercontent.com/kovidgoyal/calibre/master/setup/linux-installer.py | sudo python -c "import sys; main=lambda:sys.stderr.write('Download failed\n'); exec(sys.stdin.read()); main()"
### calibre

# dropbox
echo 'deb http://linux.dropbox.com/ubuntu precise main' >> 'dropbox.list'
chmod 644 dropbox.list
sudo chown root:root 'dropbox.list'
sudo mv 'dropbox.list' '/etc/apt/sources.list.d/dropbox.list'
sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 5044912E
sudo apt-get update && sudo apt-get install dropbox
### dropbox start -i
### dropbox stop

# encfs
sudo apt-get install encfs
### encfs ~/Dropbox/Books.enc/ ~/Dropbox-Books
### sudo umount -f ~/Dropbox-Books

# restart
source ~/.bashrc

