"""""""""
" setup "
"""""""""
if !filereadable($HOME . "/.config/nvim/autoload/plug.vim")
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
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf', { 'commit': 'e2ae1b249cf2d5258b552cfd682c7c0911981e9b' } |
Plug 'junegunn/fzf.vim', { 'commit': '0fe8e198a3a501b54dbc4f9587526c097599f95a' }
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-sneak'
Plug 'ludovicchabant/vim-gutentags'
Plug 'maximbaz/lightline-trailing-whitespace'
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
Plug 'ntpeters/vim-better-whitespace'
Plug 'pbrisbin/vim-mkdir'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
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

" live substitutions as you type
set inccommand=nosplit

" copy and paste
set clipboard+=unnamedplus

" show vertical column
set colorcolumn=81,121

" managed by lightline
set noshowmode

" turn on omnicomplete
set omnifunc=syntaxcomplete#Complete

" reduce updatetime (affects vim-gitgutter)
set updatetime=100

"""""""""""
" keymaps "
"""""""""""

" reload config file
nnoremap <Leader>R :source ~/.config/nvim/init.vim<CR>

" open config file
nnoremap <Leader>c :e ~/.config/nvim/init.vim<CR>

" unsets the 'last search pattern'
nnoremap <C-g> :noh<CR><CR>

" removes trailing spaces
nnoremap <Leader>w :StripWhitespace<CR>

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

" completion
noremap! <expr> <C-Space> "<C-x><C-o>"
noremap! <expr> <C-j> pumvisible() ? "\<C-n>" : "\<C-j>"
noremap! <expr> <C-k> pumvisible() ? "\<C-p>" : "\<C-k>"

""""""""""""""""""""""""
" plugin configuration "
""""""""""""""""""""""""

" endwise
let g:endwise_no_mappings = v:true
imap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR><Plug>DiscretionaryEnd"
imap <script> <C-X><CR> <CR><SID>AlwaysEnd

" easyalign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" fzf
let g:fzf_layout = { 'down': '40%' }
function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

nnoremap <Leader><Leader> :GitFiles --cached --others --exclude-standard<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>/ :RG<space>
nnoremap <silent> <Leader>* :Rg <C-R><C-W><CR>
vnoremap <silent> <Leader>* y:Rg <C-R>"<CR>
"" undo terminal mappings just for fzf window
au FileType fzf,Rg tnoremap <buffer> <C-h> <Left>
au FileType fzf,Rg tnoremap <buffer> <C-j> <Down>
au FileType fzf,Rg tnoremap <buffer> <C-k> <Up>
au FileType fzf,Rg tnoremap <buffer> <C-l> <Right>
au FileType fzf,Rg tnoremap <buffer> <Esc> <C-g>

"" selecting mappings
nmap <Leader><Tab> <Plug>(fzf-maps-n)
xmap <Leader><Tab> <Plug>(fzf-maps-x)
omap <Leader><Tab> <Plug>(fzf-maps-o)

" fzf-tags
nmap g] <Plug>(fzf_tags)

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
    \ 'separator': {
    \   'left': '',
    \   'right': '',
    \ },
    \ 'subseparator': {
    \   'left': '',
    \   'right': '',
    \ },
    \ }

augroup UpdateLightlineForGutentags
    autocmd!
    autocmd User GutentagsUpdating call lightline#update()
    autocmd User GutentagsUpdated call lightline#update()
augroup END

" onedark
if (has("termguicolors"))
    set termguicolors
endif
let g:onedark_terminal_italics=1
colorscheme onedark
let g:better_whitespace_guicolor = g:terminal_color_1

" rainbow
autocmd VimEnter * if &syntax != "off" | RainbowParentheses | endif

" Undotree
if !isdirectory($HOME . "/.config/nvim/undotree")
    call mkdir($HOME . "/.config/nvim/undotree", "p", 0755)
endif

nnoremap <Leader>u :UndotreeToggle<CR>
set undofile
set undodir=~/.config/nvim/undotree
let undotree_WindowLayout = 3

" sneak
let g:sneak#label = 1
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T
