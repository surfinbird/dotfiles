#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"

create_symlinks() {
    echo "Creating symlinks..."
    stuff=(".aliases .scripts .zshrc .zshenv .bash_profile \
        .bashrc .conkyrc .functions .tmux.conf \
        .Xresources.d .xsessionrc .spacemacs")

    for el in $stuff; do
        ln -fsv $(pwd)/$el ~
    done

    mkdir -p ~/.config

    for el in $(ls .config); do
        rm -f ~/.config/$el
        ln -fsv $(pwd)/.config/$el ~/.config/$el
    done
}

install_apt() {
    echo "Checking apt packages..."
    apt_dep=(zsh-antigen build-essential zsh emacs tmux vim tig silversearcher-ag xsel)

    if dpkg -l ubuntu-desktop > /dev/null 2>&1; then
        apt_dep+=(i3 i3blocks fonts-font-awesome xbacklight xss-lock)
    fi

    missing=($(comm -23 <(for i in "${apt_dep[@]}"; do echo $i; done|sort) <(dpkg -l| awk '/^i/{print $2}'|sort)))
    if [ -n "$missing" ]; then
        echo "Missing apt packages:" "${missing[@]}"
        sudo apt-get update
        sudo apt-get install -y "${missing[@]}"
    fi

    if ! which nvim 2>&1 > /dev/null; then
        echo "Installing neovim..."
        sudo add-apt-repository ppa:neovim-ppa/unstable
        sudo apt-get update
        sudo apt-get install neovim
        sudo apt-get install python-dev python-pip python3-dev python3-pip
        sudo pip3 install neovim
        # Use neovim as the default for all things vim
        sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
        sudo update-alternatives --config vi
        sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
        sudo update-alternatives --config vim
        sudo update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
        sudo update-alternatives --config editor
    fi
}

install_mac() {
    if which rg 2>&1 > /dev/null; then
        echo "Installing ripgrep..."
        brew install ripgrep
    fi

    if which nvim 2>&1 > /dev/null; then
        echo "Installing neovim..."
        brew install neovim
    fi
}

install_packages() {

    if which dpkg 2>&1 > /dev/null; then
        install_apt
    else
        install_mac
    fi

    if ! which fasd 2>&1 > /dev/null; then
        echo "Installing FASD..."
        rm -rf /tmp/fasd
        git clone https://github.com/clvv/fasd.git /tmp/fasd
        cd /tmp/fasd
        sudo make install
    fi

    if ! which fzf 2>&1 > /dev/null; then
        echo "Installing fzf..."
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
    fi

    if [ ! -d ~/.emacs.d ]; then
        git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d -b develop
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

create_symlinks
install_packages
echo "Done!"
