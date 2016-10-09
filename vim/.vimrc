"""""""""""
" plugins "
"""""""""""
" vim-plug setup
call plug#begin()
Plug 'tpope/vim-sensible'
call plug#end()

""""""""
" misc "
""""""""
" automagically remove trailing spaces
:nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" enable/disable paste mode
set pastetoggle=<F6>
" show line number
set number
