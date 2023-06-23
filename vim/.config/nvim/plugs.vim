let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()

Plug 'rakr/vim-one'
Plug 'cormacrelf/vim-colors-github'
Plug 'cocopon/iceberg.vim'
Plug 'rmehri01/onenord.nvim'

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

" git diff in the sign column
Plug 'mhinz/vim-signify'

" Easy navigating around the buffer
Plug 'easymotion/vim-easymotion'

" Protobuff highlighter
" It is fork with google updates.
Plug 'vanjiii/vim-protobuf'

" Another auto pairs plugin with nice (cs) prefix.
Plug 'tpope/vim-surround'

call plug#end()
