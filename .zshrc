# Load antigen from somewhere
if [ -f /usr/local/share/antigen/antigen.zsh ]; then
    source /usr/local/share/antigen/antigen.zsh
elif [ -f /usr/share/zsh-antigen/antigen.zsh ]; then
    source /usr/share/zsh-antigen/antigen.zsh
else
    if [ ! -f $HOME/.antigen.zsh ]; then
        curl -L git.io/antigen > $HOME/.antigen.zsh
    fi
    source $HOME/.antigen.zsh
fi

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
antigen bundle common-aliases
antigen bundle arialdomartini/oh-my-git
# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme af-magic
#antigen theme arialdomartini/oh-my-git-themes arialdo-pathinline
#antigen theme arialdomartini/oh-my-git-themes oppa-lana-style

export omg_ungit_prompt="%{\033[36m%}%~%{\033[0m%} %B%F{1}❯%F{3}❯%F{2}❯%f%b "
export omg_second_line="${omg_ungit_prompt}"

# OS specific plugins
if [[ $CURRENT_OS == 'OS X' ]]; then
    antigen bundle brew
    antigen bundle brew-cask
    antigen bundle osx
elif [[ $CURRENT_OS == 'Linux' ]]; then
    :
elif [[ $CURRENT_OS == 'Cygwin' ]]; then
    :
fi

# Tell Antigen that you're done.
antigen apply

# FASD
eval "$(fasd --init auto)"

# FZF
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Source some files
if [[ -s "${ZDOTDIR:-$HOME}/.functions" ]]; then
  source "${ZDOTDIR:-$HOME}/.functions"
fi

if [[ -s "${ZDOTDIR:-$HOME}/.aliases" ]]; then
  source "${ZDOTDIR:-$HOME}/.aliases"
fi

if [[ -s "${ZDOTDIR:-$HOME}/.aliases-this-machine" ]]; then
  source "${ZDOTDIR:-$HOME}/.aliases-this-machine"
fi

if [[ -s "${ZDOTDIR:-$HOME}/.path-setup-this-machine" ]]; then
  source "${ZDOTDIR:-$HOME}/.path-setup-this-machine"
fi

# Misc zsh setup
zstyle ':completion:*' special-dirs true

