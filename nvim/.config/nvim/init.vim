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
Plug 'easymotion/vim-easymotion'
Plug 'elzr/vim-json'
Plug 'gioele/vim-autoswap'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'luochen1990/rainbow'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
Plug 'pbrisbin/vim-mkdir'
Plug 'sheerun/vim-polyglot'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neco-syntax'
Plug 'szw/vim-tags'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/AdvancedSorters'
Plug 'w0rp/ale'
call plug#end()

""""""""""""""""""
" general config "
""""""""""""""""""
" remap leader
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
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
" window movement mappings
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" move normally by using Ctrl
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>
cnoremap <C-h> <Left>
cnoremap <C-j> <Down>
cnoremap <C-k> <Up>
cnoremap <C-l> <Right>

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" airline
let g:airline_powerline_fonts = 1
" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#keyword_patterns = {}
" easymotion
let g:EasyMotion_do_mapping = 0
nmap s <Plug>(easymotion-overwin-f)
nmap s <Plug>(easymotion-overwin-f2)
let g:EasyMotion_smartcase = 1
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" ctags
let g:vim_tags_auto_generate = 1
" fzf
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
nnoremap <C-p> :Files<cr>
nnoremap <C-b> :Buffers<cr>
nnoremap <Leader>/ :Rg<space>
nnoremap <silent> <Leader>* :Rg <C-R><C-W><CR>
vnoremap <silent> <Leader>* y:Rg <C-R>"<CR>
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
" vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" Rainbow parenthesis
let g:rainbow_active = 1
