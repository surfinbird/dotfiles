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

""" Fzf
nnoremap <C-p> :FZF<CR>

""" Airline
let g:airline_theme = 'dark'
let g:airline_powerline_fonts = 1

""" YouCompleteMe
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_min_num_identifier_candidate_chars = 4

nnoremap <leader>y :YcmForceCompileAndDiagnostics<cr>
nnoremap <leader>g :YcmCompleter GoTo<CR>
nnoremap <leader>pd :YcmCompleter GoToDefinition<CR>
nnoremap <leader>pc :YcmCompleter GoToDeclaration<CR>

""" NerdTree
nmap <leader>n :NERDTreeToggle<CR>
