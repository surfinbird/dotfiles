""" Misc

" set leader key
let mapleader=","

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

""" Appearance
set termguicolors
colorscheme badwolf

""" Syntax highlighting
au BufNewFile,BufRead SConstruct  setf python
au BufNewFile,BufRead SConscript*  setf python
