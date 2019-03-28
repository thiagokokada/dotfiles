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
Plug 'ap/vim-css-color'
Plug 'bling/vim-airline'
Plug 'brooth/far.vim'
Plug 'dietsche/vim-lastplace'
Plug 'gioele/vim-autoswap'
Plug 'janko-m/vim-test'
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
Plug 'vim-scripts/auto-pairs-gentle'
Plug 'w0rp/ale'
" clojure
Plug 'clojure-vim/acid.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'clojure-vim/async-clj-omni'
Plug 'tpope/vim-sexp-mappings-for-regular-people' | Plug 'guns/vim-sexp'
" python
Plug 'davidhalter/jedi-vim'
Plug 'zchee/deoplete-jedi'
call plug#end()

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""

" ale
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
" airline
let g:airline_powerline_fonts = 1
" deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'
" ctags
let g:vim_tags_auto_generate = 1
" far.vim
let g:far#source = 'rgnvim'
" fzf
command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
nnoremap <C-p> :GFiles<cr>
nnoremap <C-f> :Files<cr>
nnoremap <C-b> :Buffers<cr>
nnoremap <Leader>c :Commits<cr>
" gruvbox
set termguicolors
set background=dark
let g:gruvbox_italic=1
colorscheme gruvbox
" jedi
let g:jedi#smart_auto_mappings = 0
" rainbow
let g:rainbow_active = 1
" Undotree
nnoremap <F5> :UndotreeToggle<cr>
set undofile
set undodir=~/.config/nvim/undotree
let undotree_WindowLayout = 3
" vim-test
let test#strategy = 'neovim'
nnoremap <silent> <leader>t :TestNearest<CR>
nnoremap <silent> <leader>T :TestFile<CR>
nnoremap <silent> <leader>a :TestSuite<CR>
nnoremap <silent> <leader>l :TestLast<CR>
nnoremap <silent> <leader>g :TestVisit<CR>
" vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" Rainbow parenthesis
let g:rainbow_active = 1
" vim-iced
let g:iced_enable_default_key_mappings = v:true

""""""""
" misc "
""""""""
" reload config file
nnoremap <Leader>R :source ~/.config/nvim/init.vim<CR>
" unsets the "last search pattern" register by hitting return
nnoremap <CR> :noh<CR><CR>
" automagically remove trailing spaces
nnoremap <silent> <F2> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" sort in line
vnoremap <F3> d:execute 'normal i' . join(sort(split(getreg('"'))), ' ')<CR>
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
" neovim terminal
nnoremap <silent> <leader><Space> :terminal<CR>
tnoremap <Esc> <C-\><C-n>
tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
tnoremap <C-h> <C-\><C-N><C-w>h
tnoremap <C-j> <C-\><C-N><C-w>j
tnoremap <C-k> <C-\><C-N><C-w>k
tnoremap <C-l> <C-\><C-N><C-w>l
" window movement mappings
inoremap <C-h> <C-\><C-N><C-w>h
inoremap <C-j> <C-\><C-N><C-w>j
inoremap <C-k> <C-\><C-N><C-w>k
inoremap <C-l> <C-\><C-N><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
