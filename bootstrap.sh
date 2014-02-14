#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"
git pull origin master
git submodule update --init --recursive
function doIt() {
    stuff=(".emacs.d .scripts .vim .xmonad .zprezto \
        .ackrc .bash_profile .bashrc .conkyrc .functions \
        .gitconfig .rtorrent.rc .screenrc .tmux.conf .vimrc \
        .Xmodmap .Xresources .xsessionrc")
    
    for el in $stuff; do
        ln -fsv $(pwd)/$el ~
    done

#    rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" \
#        --exclude "README.md" --exclude "*.swp" --exclude ".gitmodules" \
#        -av --no-perms . ~
}
if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt
    fi
fi
unset doIt
