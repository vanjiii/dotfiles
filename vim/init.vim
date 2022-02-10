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

" drop old-time vi compatibility
set nocompatible

" create undo file by default
set undofile

" case insensitive search
set ignorecase

" syntax is ON except for golang files
syntax on
autocmd FileType go setlocal syntax=OFF

" show line numbers
set number

" yank will also copy to the system clipboard
set clipboard+=unnamedplus

" automatically removing all trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" ===========================
" Plugin specific settings
" ===========================

" => vim-go {{{
" Use new vim 8.2 pop-up windows for Go Doc
let g:go_doc_popup_window = 1

" Status line types/signatures
let g:go_auto_type_info = 1

" when option is chosen help window is closed automatically
autocmd CompleteDone * pclose
" }}}

" => Nerdtree {{{
let g:NERDTreeNodeDelimiter = "\u00a0"
let g:NERDTreeChDirMode = 2
let NERDTreeShowHidden=1
" }}}

lua << EOF
  require("which-key").setup {
    -- your configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  }
EOF

" => vim gui/neovide configs {{{
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

" => startify {{{
let g:startify_change_to_dir=0
let g:startify_change_to_vcs_root = 1
let g:startify_bookmarks = [
			\ {'d': '~/dev/src/github.com/vanjiii/dotfiles'},
			\ {'w': '~/dev/src/github.com/sumup/automatic-receipts'},
			\ ]
" }}}

" => easymotion {{{
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" }}}

" ===========================
" Local mappings
" ===========================

" Mapping selecting leader key
let mapleader = ","

map <F1> :help vnj.txt 			<CR>
map <F4> :ToggleBufExplorer 		<CR>
map <F5> :Startify 			<CR>
map <F6> :Scratch 			<CR>
map <F9> :noh 				<CR>
map <F12> :call FullscreenToggle()	<CR>

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
nnoremap <Leader><F4>	:Buffers <CR>

" NERDTree
map <F3> 		:NERDTreeToggle<CR>
map <Leader><F3> 	:NERDTreeFind<CR>

" vim-go
nnoremap <Leader>gh  	:GoSameIdsToggle<CR>
nnoremap <Leader>gt  	:GoAlternate<CR>
nnoremap <Leader>gr   	:GoRename<CR>
nnoremap <Leader>gd  	:GoInfo<CR>
nnoremap <Leader>gi  	:GoImplements<CR>
nnoremap <Leader>gf  	:GoReferrers<CR>

autocmd FileType go nmap <Leader>gb  <Plug>(go-build)
autocmd FileType go nmap <Leader>gs  <Plug>(go-run)

autocmd FileType go nmap <Leader>ds <Plug>(go-def-split)
autocmd FileType go nmap <Leader>dv <Plug>(go-def-vertical)

" easymotion

" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap <Space> <Plug>(easymotion-overwin-f2)

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
