"""""""""""
" plugins "
"""""""""""
call plug#begin()
Plug 'tpope/vim-sleuth'
Plug 'bling/vim-airline'
Plug 'vim-scripts/restore_view.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'neomake/neomake'
call plug#end()

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" Neomake
autocmd! BufWritePost * Neomake
let g:neomake_open_list = 2
" NERDTree
nnoremap <F3> :NERDTreeToggle<cr>
" Undotree
nnoremap <F4> :UndotreeToggle<cr>
let g:undotree_WindowLayout = 3
" jedi-vim
let g:jedi#smart_auto_mappings = 0

""""""""
" misc "
""""""""
" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
