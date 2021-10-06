let &runtimepath=&runtimepath . ',' . expand("<sfile>:p:h")

" load plugins with plug-vim
"
runtime! plugs.vim

" respect neovim theme rather terminal one
set termguicolors
colorscheme acme

set number
syntax on

" yank will also copy to the system clipboard
set clipboard+=unnamedplus

" Plugin specific settings

" Nerdtree hack
let g:NERDTreeNodeDelimiter = "\u00a0"

" Mapping selecting mapping

let mapleader = ","

" FZF
nnoremap gp         :Files<CR>
nnoremap <leader>p  :GFiles<CR>
nnoremap <leader>s  :BLines<CR>
nnoremap <leader>f  :Ag<CR>

" NERDTree
map <Leader>n :NERDTreeToggle<CR>
map <Leader>N :NERDTreeFind<CR>

" vim-go
nnoremap <leader>gh  :GoSameIdsToggle<CR>
nnoremap <leader>gt  :GoAlternate<CR>
nnoremap <leader>gr  :GoRename<CR>
nnoremap <leader>gd  :GoInfo<CR>
nnoremap <leader>gi  :GoImplements<CR>
nnoremap <leader>gr  :GoReferrers<CR>
