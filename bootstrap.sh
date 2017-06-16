#!/usr/bin/env bash
cd "$(dirname "${BASH_SOURCE}")"

create_symlinks() {
    echo "creating symlinks..."
    stuff=(".aliases .scripts .zshrc .zshenv .bash_profile \
        .bashrc .conkyrc .functions .tmux.conf \
        .Xresources.d .xsessionrc .spacemacs .spacemacs.d")

    for el in $stuff; do
        ln -fs $(pwd)/$el ~
    done

    mkdir -p ~/.config

    for el in $(ls .config); do
        rm -f ~/.config/$el
        ln -fs $(pwd)/.config/$el ~/.config/$el
    done
}

install_nix() {
    echo "checking apt packages..."
    apt_dep=(zsh-antigen build-essential zsh emacs tmux vim tig silversearcher-ag xsel)

    if dpkg -l ubuntu-desktop > /dev/null 2>&1; then
        apt_dep+=(i3 i3blocks fonts-font-awesome xbacklight xss-lock)
    fi

    missing=($(comm -23 <(for i in "${apt_dep[@]}"; do echo $i; done|sort) <(dpkg -l| awk '/^i/{print $2}'|sort)))
    if [ -n "$missing" ]; then
        echo "missing apt packages:" "${missing[@]}"
        sudo apt-get update
        sudo apt-get install -y "${missing[@]}"
    fi

    if ! which nvim 2>&1 > /dev/null; then
        echo "installing neovim..."
        sudo add-apt-repository ppa:neovim-ppa/unstable
        sudo apt-get update
        sudo apt-get install neovim
        sudo apt-get install python-dev python-pip python3-dev python3-pip
        sudo pip3 install neovim
        # use neovim as the default for all things vim
        sudo update-alternatives --install /usr/bin/vi vi /usr/bin/nvim 60
        sudo update-alternatives --config vi
        sudo update-alternatives --install /usr/bin/vim vim /usr/bin/nvim 60
        sudo update-alternatives --config vim
        sudo update-alternatives --install /usr/bin/editor editor /usr/bin/nvim 60
        sudo update-alternatives --config editor
    fi

    if which rg 2>&1 > /dev/null; then
        echo "installing ripgrep..."
        curl -ssf https://static.rust-lang.org/rustup.sh | sh
        cargo install ripgrep
    fi
}

install_mac() {
    if ! which rg 2>&1 > /dev/null; then
        echo "installing ripgrep..."
        brew install ripgrep
    fi

    if ! which nvim 2>&1 > /dev/null; then
        echo "installing neovim..."
        brew install neovim
    fi
}

install_packages() {

    if which dpkg 2>&1 > /dev/null; then
        install_nix
    else
        install_mac
    fi

    if ! which fasd 2>&1 > /dev/null; then
        echo "installing fasd..."
        rm -rf /tmp/fasd
        git clone https://github.com/clvv/fasd.git /tmp/fasd
        cd /tmp/fasd
        sudo make install
    fi

    if ! which fzf 2>&1 > /dev/null; then
        echo "installing fzf..."
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
    fi

    if [ ! -d ~/.emacs.d ]; then
        echo "installing spacemacs..."
        git clone git@github.com:syl20bnr/spacemacs.git ~/.emacs.d -b develop
    fi
}

case $1 in
    "--force"|"-f")
        :
        ;;
    *)
        read -p "this may overwrite existing files in your home directory. are you sure? (y/n) " -n 1
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
        ;;
esac

create_symlinks
install_packages
echo "done!"
