""" Plug
call plug#begin('~/.config/nvim/plugged')
Plug 'sjl/badwolf'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'mileszs/ack.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'tpope/vim-fugitive'
Plug 'majutsushi/tagbar', {'for': ['cpp', 'c', 'py', 'sh', 'js']}
Plug 'easymotion/vim-easymotion'
Plug 'vim-syntastic/syntastic'
call plug#end()

