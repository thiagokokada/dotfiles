""""""""""""""""""""
" install vim-plug "
""""""""""""""""""""
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

"""""""""""
" plugins "
"""""""""""
call plug#begin()
" general
Plug 'airblade/vim-gitgutter'
Plug 'dietsche/vim-lastplace'
Plug 'gioele/vim-autoswap'
Plug 'guns/vim-sexp' | Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'ludovicchabant/vim-gutentags'
Plug 'luochen1990/rainbow'
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-trailing-whitespace'
Plug 'mbbill/undotree'
Plug 'ntpeters/vim-better-whitespace'
Plug 'pbrisbin/vim-mkdir'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/AdvancedSorters'
Plug 'zackhsi/fzf-tags'
call plug#end()

""""""""""""""""""
" general config "
""""""""""""""""""

" remap leader
let g:mapleader = "\<Space>"
let g:maplocalleader = ','

" enable/disable paste mode
set pastetoggle=<F4>

" show line number
set number

" hidden unused buffers
set hidden

" live substitutions as you type
set inccommand=nosplit

" copy and paste
set clipboard=unnamedplus

" show vertical column
set colorcolumn=81,121

"""""""""""
" keymaps "
"""""""""""

" reload config file
nnoremap <Leader>R :source ~/.config/nvim/init.vim<CR>

" unsets the 'last search pattern' register by hitting return
nnoremap <CR> :noh<CR><CR>

" removes trailing spaces
noremap <Leader>w :StripWhitespace<CR>

" make Esc enter Normal mode in term
tnoremap <Esc> <C-\><C-n>
tnoremap <M-[> <Esc>
tnoremap <C-v><Esc> <Esc>

" window movement mappings
tnoremap <C-h> <c-\><c-n><c-w>h
tnoremap <C-j> <c-\><c-n><c-w>j
tnoremap <C-k> <c-\><c-n><c-w>k
tnoremap <C-l> <c-\><c-n><c-w>l
inoremap <C-h> <Esc><c-w>h
inoremap <C-j> <Esc><c-w>j
inoremap <C-k> <Esc><c-w>k
inoremap <C-l> <Esc><c-w>l
vnoremap <C-h> <Esc><c-w>h
vnoremap <C-j> <Esc><c-w>j
vnoremap <C-k> <Esc><c-w>k
vnoremap <C-l> <Esc><c-w>l
nnoremap <C-h> <c-w>h
nnoremap <C-j> <c-w>j
nnoremap <C-k> <c-w>k
nnoremap <C-l> <c-w>l

" shortcut to omnicomplete
inoremap <expr> <C-Space> "<C-x><C-o>"

" map C-j and C-k to allow moving in completion popups
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""

" easyalign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" easymotion
let g:EasyMotion_do_mapping = 0
nmap f <Plug>(easymotion-overwin-f)
nmap s <Plug>(easymotion-overwin-f2)
let g:EasyMotion_smartcase = 1
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" fzf
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
nnoremap <Leader><Leader> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>/ :Rg<space>
nnoremap <silent> <Leader>* :Rg <CR><C-W><CR>
vnoremap <silent> <Leader>* y:Rg <CR>"<CR>
"" undo terminal mappings just for fzf window
au FileType fzf,Rg tnoremap <buffer> <C-h> <Left>
au FileType fzf,Rg tnoremap <buffer> <C-j> <Down>
au FileType fzf,Rg tnoremap <buffer> <C-k> <Up>
au FileType fzf,Rg tnoremap <buffer> <C-l> <Right>
au FileType fzf,Rg tnoremap <buffer> <Esc> <C-g>

"" selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" fzf-tags
nmap <C-]> <Plug>(fzf_tags)

" gutentags
let g:gutentags_cache_dir="~/.config/nvim/gutentags"
let g:gutentags_file_list_command = {
    \ 'markers': {
    \   '.git': 'git ls-files',
    \   '.hg': 'hg files',
    \ },
    \ }

" lightline
let g:lightline = {
    \ 'colorscheme': 'onedark',
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'filename', 'readonly', 'modified' ],
    \             [ 'gitbranch' ],
    \           ],
    \   'right': [
    \             [ 'trailing' ],
    \             [ 'percent' ],
    \             [ 'lineinfo' ],
    \             [ 'fileformat', 'fileencoding' ],
    \             [ 'gutentags'],
    \            ],
    \ },
    \ 'component_expand': {
    \   'trailing': 'lightline#trailing_whitespace#component',
    \ },
    \ 'component_function': {
    \   'gitbranch': 'fugitive#head',
    \   'gutentags': 'gutentags#statusline',
    \   'trailing': 'lightline#trailing_whitespace#component'
    \ },
    \ 'component_type': {
    \   'trailing': 'error'
    \ },
    \ }

" onedark
set termguicolors
syntax on
let g:onedark_terminal_italics=1
colorscheme onedark
let g:better_whitespace_guicolor = g:terminal_color_1

" rainbow
let g:rainbow_active = 1

" Undotree
nnoremap <Leader>u :UndotreeToggle<CR>
set undofile
set undodir=~/.config/nvim/undotree
let undotree_WindowLayout = 3
