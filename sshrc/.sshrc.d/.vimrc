noremap d h
noremap h gj
noremap t gk
noremap n l
inoremap <silent> hh <ESC>

inoremap <C-c> <ESC>
noremap! <C-g> <C-c>

noremap k d
noremap K D 
noremap m n
noremap! <C-b> <BS>

noremap! <C-h> <Down>
noremap! <C-t> <Up>
noremap! <C-d> <Left>
noremap! <C-n> <Right>

let mapleader = "\<Space>"
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>b :Denite buffer<CR>
nnoremap <Leader>f :DeniteBufferDir file_rec<CR>

augroup netrw_dvorak_fix
    autocmd!
    autocmd filetype netrw call Fix_netrw_maps_for_dvorak()
augroup END
function! Fix_netrw_maps_for_dvorak()
    noremap <buffer> d h
    noremap <buffer> h gj
    noremap <buffer> t gk
    noremap <buffer> n l
    noremap <buffer> e t 
    noremap <buffer> l n
    " and any others...
endfunction

set clipboard=unnamedplus

set hidden

set number
set relativenumber

set expandtab 
set tabstop=4
set shiftwidth=4
set autoindent
set mouse=a