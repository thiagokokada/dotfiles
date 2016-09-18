"""""""""""
" plugins "
"""""""""""
call plug#begin()
Plug 'tpope/vim-sleuth'
Plug 'bling/vim-airline'
Plug 'vim-scripts/restore_view.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'neomake/neomake'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'zchee/deoplete-jedi', { 'for': 'python'}
call plug#end()

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" Neomake
autocmd! BufWritePost * Neomake
let g:neomake_open_list = 2
let g:neomake_python_flake8_maker = { 'args': ['--ignore=E115,E266,E501'], }
let g:neomake_python_pylint_maker = { 'args': ['--ignore=missing-docstring'], }
" NERDTree
nnoremap <F3> :NERDTreeToggle<cr>
" Undotree
nnoremap <F4> :UndotreeToggle<cr>
let g:undotree_WindowLayout = 3
" jedi-vim
let g:jedi#smart_auto_mappings = 0
" deoplete
let g:deoplete#enable_at_startup = 1

""""""""
" misc "
""""""""
" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
