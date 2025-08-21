local set = vim.opt

set.statusline = "%f%m%r%h%w %q %=%-14.(%l:%c%V%) %{v:lua.gitbranch()} %y [%{empty(&fileencoding)?'enc':&fileencoding}] %{&expandtab?'spaces:'.&tabstop:'tabs:'.&tabstop} %P "

set.cursorline = true

-- set numbers
set.number = true
set.relativenumber = true

-- show existing tabs with 4 spaces
set.tabstop = 4
set.softtabstop = 4
set.shiftwidth = 4
-- replace tab with spaces
-- set.expandtab = true

set.wrap = false

-- don't go to end of file but to the N rows
set.scrolloff = 8

set.hlsearch = true
set.incsearch = true

set.swapfile = false
set.backup = false

set.listchars = {
	eol = '↵',
	tab = '>-',
	space = ' ',
	trail = '-',
	extends = '#',
}
set.list = false

-- spell checker
set.spell = true
