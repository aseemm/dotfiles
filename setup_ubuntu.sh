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

# python3, flask (within venv)
   87  sudo apt-get install curl
   93  sudo apt-get install python3
   95  python3 -m venv flask --without-pip
   97  source flask/bin/activate
  100  cd flask
  101  mkdir pypioffline
  102  cd pypioffline/
  105  curl -O https://pypi.python.org/packages/source/s/setuptools/setuptools-6.1.tar.gz
  106  tar xvzf setuptools-6.1.tar.gz 
  107  cd setuptools-6.1/
  108  python ez_setup.py
  109  easy_install pip

  111  pip install flask
  112  pip install flask-login
  113  pip install flask-openid
  114  pip install flask-mail
  115  pip install flask-sqlalchemy
  116  pip install sqlalchemy-migrate
  117  pip install flask-whooshalchemy
  118  pip install flask-wtf
  119  pip install flask-babel
  120  pip install guess_language
  121  pip install flipflop
  122  pip install coverage
