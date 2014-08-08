# Git Guide

### Create a new repo
git init
git add .
git commit -m "first commit"
git remote add origin https://github.com/aseemm/jenkins-style.git
git push -u origin master

### Clone an existing repo
git clone https://github.com/aseemm/dotfiles.git .dotfiles  

### Misc
git init # initializes a new repo
git clone # creates a copy of an existing repo
git config # setup configuration options for git installation

git add/rm guides/git.md # move changes to staging snapshot
git commit # takes staged snapshot and commits it to project history
git commit -a # commit with add

git status # displays state of wokring directory, and staged snapshot
git log # explore previous revisions of project

git checkout -- guides/git.md # revert local changes
git revert # revert a committed snapshot
git reset # revert local chanages
git clean # remove untracked files

git branch # create a branch
git checkout # navigate between branches
git merge # merge between branches

git remote # administer remote connections
git fetch # fetch, no integrate
git pull # fetch with merge
git push # move to another repo

git diff # diff between working directory and index
git diff -cached # diff between most recent commit and index
git diff HEAD # diff betwen working directory and most recent commit