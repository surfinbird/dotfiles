#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"
git pull origin master
git submodule update --init --recursive
function doIt() {
    stuff=(".aliases .scripts .vim .xmonad .zprezto \
        .ackrc .bash_profile .bashrc .conkyrc .functions \
        .gitconfig .rtorrent.rc .screenrc .tmux.conf .vimrc \
        .Xmodmap .Xresources .Xresources.d .xsessionrc .xmobarrc \
        .spacemacs")
    
    for el in $stuff; do
        ln -fsv $(pwd)/$el ~
    done

    cp -vR .config ~/

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

if [ ! -d ~/.emacs.d ]; then
    git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d
fi
