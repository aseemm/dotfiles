# Git Guide

### Concepts/Ideas
Files in [ Working Directory OR Staging Area OR Repositiory ]
Keep commit Messages as "commands" - "add" rather than "added"
Commit Labels are Branches
"remote branch" - branch created by someone else
"Commits" have parents

# Settings
Download git-completion.bash, git-prompt.sh from https://github.com/git/git/contrib/completion, update .bashrc/.bashprofile
git config --global core.editor "emacs"
                    push.default upstream
                    merge.confictstyle diff3

### Create a new repo
git init # initialize a new repo
git add .
git commit -m "first commit"
git remote add origin https://github.com/aseemm/jenkins-style.git # git remote add origin https://aseemm@bitbucket.org/aseemm/d3.git
git push -u origin master

### Clone an existing repo
git clone https://github.com/aseemm/dotfiles.git .dotfiles # automatically sets up remote

### Misc
git config # setup configuration options for git installation

git add/rm guides/git.md # move from Working Directory to Staging Area
git commit # move from Staging Area to Repository
git commit -a # move from Working Directory to Repository, directly

git status # displays state of Working Directory and Staging Area
git log # explore previous revisions of project
git log --graph --oneline b1 b2 # visualize branches
git log -n1 # show last 1 commit
git log --stat

git checkout -- guides/git.md # revert local changes
git revert # revert a committed snapshot
git checkout <commitid> # revert
git reset # revert local changes
git reset --hard # discard Working Directory/Staging Area changes
git clean # remove untracked files

git branch # shows branches
git branch <blabel> # create a branch
git checkout <blabel> # use branch
git merge master <blabel> # merge blabel (branch) into master
git branch -d <blabel> # delete branch
git checkout -b <label> # create and checkout branch

git remote # administer/see remote connections
git fetch # fetch, no integrate
git pull # from remote
git push # to remote

git diff # diff between Working Directory & Staging Area
git diff --staged # diff between Staging Area & Repository
git difftool
git diff -cached # diff between most recent commit and index
git diff HEAD # diff betwen working directory and most recent commit
git show <commitid> # diff with parent
git diff <oldcommitid> <newcommitid>

## login using https protocol
git remote set-url origin https://aseemm@github.com/aseemm/dotfiles.git

## changing remote
git remote -v
git remote set-url origin https://aseemm@github.com/aseemm/dotfiles.git
