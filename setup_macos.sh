#!/bin/bash
ln -s ~/.dotfiles/.emacs ~/.emacs
ln -s ~/.dotfiles/elisp ~/elisp
ln -s ~/.dotfiles/.bashrc ~/.bashrc
ln -s ~/.dotfiles/.bashrc_custom ~/.bashrc_custom

# Install xcode, from App Store
xcode-select --install
xcode-select -p

# macports, https://www.macports.org/install.php
# Install macports 

# encfs, http://emmanuelbernard.com/blog/2013/07/01/encrypt-your-dropbox-with-encfs-on-mac-os-x/
# sudo port install encfs

# emacs-app, http://wikemacs.org/wiki/Installing_Emacs_on_OS_X
# sudo port install emacs-app

Install emacs from https://emacsformacosx.com/
/Applications/Emacs.app/Contents/MacOS/Emacs -nw
