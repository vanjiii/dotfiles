let &runtimepath=&runtimepath . ',' . expand("<sfile>:p:h")

" load plugins with plug-vim
runtime! plugs.vim

" respect neovim theme rather terminal one
set termguicolors
set background=light
colo onenord-light

" Proposed by neovim#healthcheck
let g:python3_host_prog = '/usr/bin/python'

" enable mouse by default
set mouse=a

" Disable swapfiles
set noswapfile

" drop old-time vi compatibility
set nocompatible
filetype plugin on

" Activate builtin plugin matchit. It enhances the % command (e.g. can jump
" between html tags or ruby if-end blocks).
runtime macros/matchit.vim

" create undo file by default
set undofile

" Case insensitive search.
" This also affects the text autocompletion.
set ignorecase
set smartcase

" those are on by default on Neovim but off on Vim.
set incsearch
set hlsearch
set wrapscan

" syntax is ON except for golang files
syntax on
autocmd FileType go setlocal syntax=OFF

" show line numbers
set number

" Explicitly set autoindent to true.
set autoindent

" set OS clipboard paste available in insert mode.
set pastetoggle=<f10>

" automatically removing all trailing whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" set tab width equal to 2 spaces for markdown files.
autocmd Filetype markdown setlocal shiftwidth=2

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

" make golangci-lint default linter => :GoMetaLinter will run it.
let g:go_metalinter_enabled = []
let g:go_metalinter_command = 'golangci-lint'
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
  }
EOF

" => easymotion {{{
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" }}}
"

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" ===========================
" Local mappings
" ===========================

map <F1> :help vnj.txt 			<CR>
map <F6> :Scratch 			<CR>
map <F9> :noh 				<CR>

" Quicklist and Location list windows
nnoremap <Leader>l[	:lprevious	<CR>
nnoremap <Leader>l]	:lnext		<CR>
nnoremap <Leader>lc	:lclose		<CR>

nnoremap <Leader>c[	:cprevious	<CR>
nnoremap <Leader>c]	:cnext		<CR>
nnoremap <Leader>cc	:cclose		<CR>

" FZF
nnoremap <Leader><Leader> 	:Files<CR>
nnoremap <C-p> 			:Files<CR>
nnoremap <Leader>pg  		:GFiles?<CR>
nnoremap <Leader>s  		:BLines<CR>
nnoremap <Leader>pf  		:Ag<CR>
nnoremap <silent> <Leader>pF 	:Ag <C-R><C-W><CR>
nnoremap <F4>			:Buffers <CR>

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

autocmd FileType go nmap <Leader>gb <Plug>(go-build)
autocmd FileType go nmap <Leader>gs <Plug>(go-run)

autocmd FileType go nmap <Leader>ds <Plug>(go-def-split)
autocmd FileType go nmap <Leader>dv <Plug>(go-def-vertical)

autocmd FileType go nmap <Leader>gg :call go#lsp#Restart()<CR>

" easymotion

" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap <Space> <Plug>(easymotion-overwin-f2)

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
