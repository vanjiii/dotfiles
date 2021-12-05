let &runtimepath=&runtimepath . ',' . expand("<sfile>:p:h")

" load plugins with plug-vim
runtime! plugs.vim

" respect neovim theme rather terminal one
set termguicolors
set background=light
colo iceberg

" enable mouse by default
set mouse=a

" syntax is on except for golang files
syntax on
" autocmd FileType go setlocal syntax=OFF

" show line numbers
set number

" yank will also copy to the system clipboard
set clipboard+=unnamedplus

" automatically removing all trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" Plugin specific settings

" Nerdtree hack
let g:NERDTreeNodeDelimiter = "\u00a0"
let g:NERDTreeChDirMode = 2
let NERDTreeShowHidden=1

lua << EOF
  require("which-key").setup {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  }
EOF

" Mapping selecting mapping
let mapleader = ","

map <F1> :help vnj.txt <CR>

map <F9> :noh <CR>

map <F6> :Scratch <CR>

" FZF
nnoremap gp         	:Files<CR>
nnoremap <Leader>p  	:GFiles<CR>
nnoremap <Leader>s  	:BLines<CR>
nnoremap <Leader>f  	:Ag<CR>

" NERDTree
map <F3> 		:NERDTreeToggle<CR>
map <Leader><F3> 	:NERDTreeFind<CR>

" vim-go
nnoremap <Leader>gh  	:GoSameIdsToggle<CR>
nnoremap <Leader>gt  	:GoAlternate<CR>
nnoremap <Leader>r   	:GoRename<CR>
nnoremap <Leader>gd  	:GoInfo<CR>
nnoremap <Leader>gi  	:GoImplements<CR>
nnoremap <Leader>gr  	:GoReferrers<CR>
