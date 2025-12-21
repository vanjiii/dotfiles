local set = vim.opt

-- TODO: make lualine to be custom for non-editable buffers (like nvimtree)
set.statusline =
"%f%m%r%h%w %q %=%-14.(%l:%c%V%) %{v:lua.gitbranch()} %y [%{empty(&fileencoding)?'enc':&fileencoding}] %{&expandtab?'spaces:'.&tabstop:'tabs:'.&tabstop} %P "

set.cursorline = true

-- set numbers
set.number = true
set.relativenumber = true

--
-- show existing tabs with 4 spaces
--
set.tabstop = 4        -- how many spaces a <Tab> counts for
set.softtabstop = 4    -- how many spaces when you press <Tab> in insert mode
set.shiftwidth = 4     -- how many spaces to use for autoindent
set.expandtab = true   -- replace tab with spaces
set.smartindent = true -- auto-indent new lines
set.autoindent = true  -- copy indent from current line when starting a new one

set.wrap = false

-- don't go to end of file but to the N rows
set.scrolloff = 8

set.hlsearch = true
set.incsearch = true

set.swapfile = false
set.backup = false

-- TODO: make `set.list = true` then just swap between diff listchars (this and ' ' for all except extends and precedes)
set.listchars = {
	eol = '↵',
	tab = '>-',
	space = ' ',
	trail = '-',
	extends = "⟩", -- when line continues right
	precedes = "⟨", -- when line continues left
	nbsp = "␣", -- non-breaking space
}
set.list = false

-- spell checker
set.spell = true
