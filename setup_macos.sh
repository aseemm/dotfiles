#!/bin/bash
ln -s ~/.dotfiles/.emacs ~/.emacs
ln -s ~/.dotfiles/elisp ~/elisp

# xcode, from App Store
Install Xcode
  502  xcode-select --install
  503  xcode-select -p

# macports, https://www.macports.org/install.php
Install macports 

# encfs, http://emmanuelbernard.com/blog/2013/07/01/encrypt-your-dropbox-with-encfs-on-mac-os-x/
sudo port install encfs

# emacs-app, http://wikemacs.org/wiki/Installing_Emacs_on_OS_X
sudo port install emacs-app
