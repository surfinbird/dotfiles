#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"

update_repos() {
    echo "-- Update repositories --"
    git pull origin master
    git submodule update --init --recursive

    if [ ! -d ~/.emacs.d ]; then
        git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d
    fi
}

create_symlinks() {
    echo "-- Create symlinks to config files --"
    stuff=(".aliases .scripts .vim .zprezto .ackrc .bash_profile \
        .bashrc .conkyrc .functions .gitconfig .tmux.conf .vimrc \
        .Xmodmap .Xresources.d .xsessionrc .spacemacs")

    for el in $stuff; do
        ln -fsv $(pwd)/$el ~
    done

    for el in $(ls .config); do
        rm -f ~/.config/$el
        ln -fsv $(pwd)/.config/$el ~/.config/$el
    done
}

install_packages() {
    if ! which apt 2>&1 > /dev/null; then
        # not on ubuntu/debian
        return
    fi

    echo "-- Checking Apt packages --"
    apt_dep=(build-essential zsh emacs tmux vim scrot i3 suckless-tools conky tig)
    # lxappearance gtk-chtheme qt4-qtconfig qt5-qtconfig
    missing=($(comm -23 <(for i in "${apt_dep[@]}"; do echo $i; done|sort) <(dpkg -l| awk '/^i/{print $2}'|sort)))
    if [ -n "$missing" ]; then
        echo "Missing apt packages:" "${missing[@]}"
        sudo apt-get update
        sudo apt-get install -y "${missing[@]}"
    fi

    echo "-- Checking for FASD --"
    if ! which fasd 2>&1 > /dev/null; then
        rm -rf /tmp/fasd
        git clone https://github.com/clvv/fasd.git /tmp/fasd
        cd /tmp/fasd
        sudo make install
    fi
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
install_packages
echo "-- Done --"
