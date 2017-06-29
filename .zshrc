if [ -f /usr/local/share/antigen/antigen.zsh ]; then
    source /usr/local/share/antigen/antigen.zsh
fi

if [ -f /usr/share/zsh-antigen/antigen.zsh ]; then
    source /usr/share/zsh-antigen/antigen.zsh
fi

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
antigen bundle common-aliases

# Show hostname on remote systems
if [[ -n "$SSH_CLIENT" ]]; then
    RPROMPT='%{$fg_bold[white]%}%M'
fi

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
# antigen theme robbyrussell
#antigen theme gnzh
antigen theme af-magic

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

# FASD
eval "$(fasd --init auto)"

# fzf
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

# Tell Antigen that you're done.
antigen apply
