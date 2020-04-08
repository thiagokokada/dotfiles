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
Plug 'bling/vim-airline'
Plug 'dietsche/vim-lastplace'
Plug 'gioele/vim-autoswap'
Plug 'guns/vim-sexp' | Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'luochen1990/rainbow'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
Plug 'pbrisbin/vim-mkdir'
Plug 'sheerun/vim-polyglot'
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/neco-syntax'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/AdvancedSorters'
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
fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun
noremap <Leader>w :call TrimWhitespace()<CR>

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
inoremap <expr> <Esc> pumvisible() ? "\<C-e>" : "\<Esc>"
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
inoremap <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
inoremap <expr> <PageDown> pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<PageDown>"
inoremap <expr> <PageUp> pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<PageUp>"

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" airline
let g:airline_powerline_fonts = 1

" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 400

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
nnoremap <Leader><Leader> :Files<cr>
nnoremap <Leader>b :Buffers<cr>
nnoremap <Leader>/ :Rg<space>
nnoremap <silent> <Leader>* :Rg <C-R><C-W><CR>
vnoremap <silent> <Leader>* y:Rg <C-R>"<CR>
" undo mappings just for fzf window
au FileType fzf,Rg tnoremap <buffer> <C-h> <Left>
au FileType fzf,Rg tnoremap <buffer> <C-j> <Down>
au FileType fzf,Rg tnoremap <buffer> <C-k> <Up>
au FileType fzf,Rg tnoremap <buffer> <C-l> <Right>
au FileType fzf,Rg tnoremap <buffer> <Esc> <c-g>

" gruvbox
set termguicolors
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox

" rainbow
let g:rainbow_active = 1

" Undotree
nnoremap <Leader>u :UndotreeToggle<cr>
set undofile
set undodir=~/.config/nvim/undotree
let undotree_WindowLayout = 3
