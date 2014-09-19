# Install - Mac OS X (partly from http://emmanuelbernard.com/blog/2013/07/01/encrypt-your-dropbox-with-encfs-on-mac-os-x/)

Install Xcode
  502  xcode-select --install
  503  xcode-select -p
Install macports (https://www.macports.org/install.php)
## Install MacFUSE (https://code.google.com/p/macfuse/downloads/detail?name=MacFUSE-2.0.3%2C2.dmg&can=2&q=)
## Install osxfuse
## Install TrueCrypt
Install EncFs
  522  sudo port install encfs
  523  sudo port install emacs-app (http://wikemacs.org/wiki/Installing_Emacs_on_OS_X)

# Usage

### Create a new voluem
encfs ~/Dropbox/Private.enc/ ~/Private/ # mounts the encrypted ~/Dropbox/Private.enc directory as ~/Private/ and under the volume name Dropbox private

To store EncFS on a file system that is case insensitive for lookup,
(VFAT, NTFS, HFS+, etc), create it using expert mode and select B32Block or
B32Stream as the filename cipher to use
Using cipher Blowfish, key size 160, block size 512

# Unmount
umount ~/Dropbox/Private.enc/ 