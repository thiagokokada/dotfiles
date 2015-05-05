set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" installed plugins
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-sleuth'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-rails.git'
Plugin 'davidhalter/jedi-vim'
Plugin 'bling/vim-airline'
Plugin 'kien/ctrlp.vim'
Plugin 'mbbill/undotree'
Plugin 'plasticboy/vim-markdown'
Plugin 'chrisbra/csv.vim'
Plugin 'vim-scripts/restore_view.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Plugins' configuration
nnoremap <F3> :NERDTreeToggle<cr>
nnoremap <F4> :UndotreeToggle<cr>
let g:undotree_WindowLayout = 3
let g:csv_autocmd_arrange = 1
set viewoptions=cursor,folds,slash,unix
let g:skipview_files = ['*\.vim']

" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" For all text files set 'textwidth' to 78 characters.
autocmd FileType text setlocal textwidth=78

set number			" show line number
set background=dark		" default terminal background
set clipboard=unnamedplus	" alias unnamed register to the + register
set smartcase			" only be case sensitive when it matters!
set mouse=a                     " in most terminal emulators this works fine!
set lazyredraw                  " make vim faster(?)
set ttyfast
