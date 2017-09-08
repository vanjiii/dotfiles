" Always wrap long lines:
set wrap

"Add line numbers
set nu

" Change the tab and the EOL symbols(Does not work in windows...) 
"set listchars=tab:â–¸\ ,eol:Â
"set list

"Highlight the tab and the EOL symbols to more accurate color
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=#4a4a59

" Add theme | under windows
syntax on 
colorscheme desert

"Set font | under windows
set guifont=Consolas:h10 

"Startup maximize | under windows
au GUIEnter * simalt ~x 
