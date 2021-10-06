let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
 
call plug#begin()

" Nord theme
" Plug 'casonadams/nord.vim'
Plug 'plan9-for-vimspace/acme-colors'
" 
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" Plug 'liuchengxu/vim-clap'
" 
" " fuzzy file, buffer, mru, tag, etc finder
" Plug 'ctrlpvim/ctrlp.vim'
 
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

Plug 'preservim/nerdtree' 

call plug#end()
