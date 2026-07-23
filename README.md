# dotfiles

Homebin like https://coreyja.com/dotfiles-git-in-home-dir

The magic is all in `.gitignore`, which ignores everything in $HOME except whatever you `git add
-f` explicitly or add to `.gitignore` explicitly.

# 1. Clone (bare or normal, doesn't matter) into a temp location
git clone --no-checkout git@github.com:you/dotfiles.git /tmp/dotfiles

# 2. Move just the .git directory into your home dir
mv /tmp/dotfiles/.git ~/.git
rm -rf /tmp/dotfiles

# 3. From your home directory, check out the tracked files
cd ~
git checkout main