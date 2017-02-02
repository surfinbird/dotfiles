" ==========================================================
" Vundle
" ==========================================================
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'crusoexia/vim-monokai'

call vundle#end()            " required
filetype plugin indent on    " required

" ==========================================================
" CtrlP
" ==========================================================
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_show_hidden = 1
let g:ctrlp_max_files = 0
if filereadable("~/.ctrlp_custom_ignore.vim")
    source ~/.ctrlp_custom_ignore.vim
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" YouCompleteMe                              "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_min_num_identifier_candidate_chars = 4
""let g:ycm_extra_conf_globlist = ['~/repos/*']
""let g:ycm_filetype_specific_completion_to_disable = {'javascript': 1}
""let g:ycm_rust_src_path = $HOME . '/repos/rust/src'

nnoremap <leader>y :YcmForceCompileAndDiagnostics<cr>
nnoremap <leader>g :YcmCompleter GoTo<CR>
nnoremap <leader>pd :YcmCompleter GoToDefinition<CR>
nnoremap <leader>pc :YcmCompleter GoToDeclaration<CR>
