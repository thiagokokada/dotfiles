"""""""""""
" plugins "
"""""""""""
call plug#begin()
Plug 'airblade/vim-gitgutter'
Plug 'bling/vim-airline'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'dietsche/vim-lastplace'
Plug 'jiangmiao/auto-pairs'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'morhetz/gruvbox'
Plug 'neomake/neomake'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'zchee/deoplete-jedi', { 'for': 'python' }
call plug#end()

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" deoplete
let g:deoplete#enable_at_startup = 1
" gruvbox
set termguicolors
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox
" jedi-vim
let g:jedi#smart_auto_mappings = 0
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

""""""""
" misc "
""""""""
" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
" show vertical column
set colorcolumn=81,121
