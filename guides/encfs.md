# Install - Mac OS X (partly from http://emmanuelbernard.com/blog/2013/07/01/encrypt-your-dropbox-with-encfs-on-mac-os-x/)

Install Xcode
  502  xcode-select --install
  503  xcode-select -p
Install macports (https://www.macports.org/install.php)
  510  export PATH=$PATH:/opt/local/bin
## Install MacFUSE (https://code.google.com/p/macfuse/downloads/detail?name=MacFUSE-2.0.3%2C2.dmg&can=2&q=)
## Install osxfuse (https://osxfuse.github.io/)
## Install TrueCrypt
export PATH=$PATH:/opt/local/bin
Install EncFs
  522  sudo port install encfs

# Usage

### Create a new volume
# mounts the encrypted ~/Dropbox/Private.enc directory as ~/Private/ and under the volume name Dropbox private
Work Macbook - encfs ~/Dropbox/Private.enc/ ~/Dropbox-Private/ 
Home Macbook - encfs ~/Documents/Dropbox/Private.enc/ ~/Dropbox-Private/ 

To store EncFS on a file system that is case insensitive for lookup,
(VFAT, NTFS, HFS+, etc), create it using expert mode and select B32Block or
B32Stream as the filename cipher to use
Using cipher Blowfish, key size 160, block size 512

# Unmount
umount ~/Dropbox/Private.enc/ 
umount ~/Documents/Dropbox/Private.enc/ 