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
Plug 'brooth/far.vim'
Plug 'dietsche/vim-lastplace'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf' | Plug 'junegunn/fzf.vim'
Plug 'mbbill/undotree'
Plug 'morhetz/gruvbox'
Plug 'neomake/neomake'
Plug 'Raimondi/delimitMate'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'szw/vim-tags'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
" python
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
" ruby
Plug 'itmammoth/run-rspec.vim', { 'for': 'ruby' }
Plug 'thoughtbot/vim-rspec', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'zchee/deoplete-jedi', { 'for': 'python' }
" scss
Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
call plug#end()

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""
" airline
let g:airline_powerline_fonts = 1
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
let g:neomake_ruby_enabled_makers = ['mri']
" Undotree
nnoremap <F4> :UndotreeToggle<cr>
set undofile
set undodir=~/.config/nvim/undotree
let undotree_WindowLayout = 3
" fzf
nnoremap <C-p> :Files<cr>
nnoremap <Leader>c :Commits<cr>
" ctags
let g:vim_tags_auto_generate = 1
" vim-rspec
nnoremap <Leader>t :call RunCurrentSpecFile()<CR>
nnoremap <Leader>s :call RunNearestSpec()<CR>
nnoremap <Leader>l :call RunLastSpec()<CR>
nnoremap <Leader>a :call RunAllSpecs()<CR>
let g:rspec_command = "!bundle exec rspec {spec}"

""""""""
" misc "
""""""""
" automagically remove trailing spaces
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
" global registers
set clipboard=unnamedplus
" show vertical column
set colorcolumn=81,121
