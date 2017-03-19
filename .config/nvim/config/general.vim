""" Misc

" set leader key
let mapleader=","

" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

""" Keyboard shortcuts

noremap <S-Left> :cp<CR>
noremap <S-Right> :cn<CR>

noremap <C-Right> :bnext<CR>
noremap <C-Left> :bprevious<CR>

" Quit window on <leader>q
nnoremap <leader>q :q<CR>

" hide matches on <leader>space
nnoremap <leader><space> :nohlsearch<cr>

" Remove trailing whitespace on <leader>S
nnoremap <leader>S :%s/\s\+$//<cr>:let @/=''<CR>

""" Appearance
set termguicolors
colorscheme badwolf

""" Syntax highlighting
au BufNewFile,BufRead SConstruct  setf python
au BufNewFile,BufRead SConscript*  setf python

""" Clipboard
set clipboard+=unnamedplus

""" Misc ...
syntax on                  " syntax highlighing
filetype on                " try to detect filetypes
filetype plugin indent on  " enable loading indent file for filetype
set number                 " Display line numbers
set numberwidth=1          " using only 1 column (and 1 space) while possible
set notitle                " (do not) show title in console title bar
set cursorline             " have a line indicate the cursor location
set scrolloff=3            " Keep 3 context lines above and below the cursor
set showmatch              " Briefly jump to a paren once it's balanced
set nowrap                 " don't wrap text
set linebreak              " don't wrap textin the middle of a word
set smartindent            " use smart indent if there is no indent file
set tabstop=4              " <tab> inserts 4 spaces 
set shiftwidth=4           " but an indent level is 2 spaces wide.
set softtabstop=4          " <BS> over an autoindent deletes both spaces.
set expandtab              " Use spaces, not tabs, for autoindent/tab key.
set shiftround             " rounds indent to a multiple of shiftwidth
set ignorecase             " Default to using case insensitive searches,
set smartcase              " unless uppercase letters are used in the regex.

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
let NERDTreeShowHidden=1

""" Tagbar
nmap <F8> :TagbarToggle<CR>

""" EasyMotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
"nmap s <Plug>(easymotion-overwin-f)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap s <Plug>(easymotion-overwin-f2)

" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

""" Ack
let g:ackprg = 'rg --vimgrep --no-heading'
" Do not jump to first result automatically
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

