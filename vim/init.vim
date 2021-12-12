let &runtimepath=&runtimepath . ',' . expand("<sfile>:p:h")

" load plugins with plug-vim
runtime! plugs.vim

" respect neovim theme rather terminal one
set termguicolors
set background=light
colo iceberg

" enable mouse by default
set mouse=a

" Disable swapfiles
set noswapfile

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

" vim-go {{{
" Use new vim 8.2 popup windows for Go Doc
let g:go_doc_popup_window = 1

" Status line types/signatures
let g:go_auto_type_info = 1

" when option is choosen help window is closed automatically
autocmd CompleteDone * pclose
" }}}

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

" vim gui/neovide configs {{{
set guifont=Fira\ Code:h10
let g:neovide_refresh_rate=60
let g:neovide_fullscreen=v:false
let g:neovide_cursor_antialiasing=v:true
" }}}

function! FullscreenToggle()
    if g:neovide_fullscreen
	let g:neovide_fullscreen=v:false
    else
	let g:neovide_fullscreen=v:true
    endif
endfunction

" Mapping selecting mapping
let mapleader = ","

map <F1> :help vnj.txt <CR>
map <F4> :ToggleBufExplorer <CR>
map <F6> :Scratch <CR>
map <F9> :noh <CR>
map <F12> :call FullscreenToggle()<CR>

" Quicklist and Location list windows
nnoremap <Leader>lp	:lprevious	<CR>
nnoremap <Leader>ln	:lnext		<CR>
nnoremap <Leader>lc	:lclose		<CR>

nnoremap <Leader>cp	:cprevious	<CR>
nnoremap <Leader>cn	:cnext		<CR>
nnoremap <Leader>cc	:cclose		<CR>

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

autocmd FileType go nmap <Leader>b  <Plug>(go-build)
autocmd FileType go nmap <Leader>r  <Plug>(go-run)

autocmd FileType go nmap <Leader>ds <Plug>(go-def-split)
autocmd FileType go nmap <Leader>dv <Plug>(go-def-vertical)
