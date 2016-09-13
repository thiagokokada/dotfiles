"""""""""""
" plugins "
"""""""""""
" vim-plug setup
call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'bling/vim-airline'
Plug 'vim-scripts/restore_view.vim'
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
call plug#end()

" plugin configuration
nnoremap <F3> :NERDTreeToggle<cr>
nnoremap <F4> :UndotreeToggle<cr>
let g:undotree_WindowLayout = 3

""""""""
" misc "
""""""""
" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
