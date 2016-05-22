#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"

update_repos() {
    git pull origin master
    git submodule update --init --recursive
    
    if [ ! -d ~/.emacs.d ]; then
        git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d
    fi
}

create_symlinks() {
    stuff=(".aliases .scripts .vim .xmonad .zprezto \
        .ackrc .bash_profile .bashrc .conkyrc .functions \
        .gitconfig .rtorrent.rc .screenrc .tmux.conf .vimrc \
        .Xmodmap .Xresources .Xresources.d .xsessionrc .xmobarrc \
        .spacemacs .i3blocks.conf")

    for el in $stuff; do
        ln -fsv $(pwd)/$el ~
    done

    for el in $(ls .config); do
        ln -fsv $(pwd)/.config/$el ~/.config/$el
    done
}

case $1 in
    "--force"|"-f")
        :
        ;;
    *)
        read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
        ;;
esac

update_repos
create_symlinks

