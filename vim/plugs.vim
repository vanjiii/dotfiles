let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'rakr/vim-two-firewatch'
Plug 'rakr/vim-one'
Plug 'cormacrelf/vim-colors-github'
Plug 'cocopon/iceberg.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

Plug 'preservim/nerdtree'

" helper window which shows available keybindings available of a key
Plug 'folke/which-key.nvim'

" comment stuff out
Plug 'tpope/vim-commentary'

" emacs like scratch buffer
Plug 'vim-scripts/scratch.vim'

" Buffer explorer plugin
Plug 'jlanzarotta/bufexplorer'

call plug#end()
