-- I don't care about vim syntax highlight.
vim.cmd("syntax off")

-- TODO: make TODO, FIXME and other to be highlighted red+italic
require('nvim-treesitter').install({
	"bash",
	"css",
	"dockerfile",
	"go",
	"html",
	"java",
	"javascript",
	"json",
	"lua",
	"markdown",
	"markdown_inline",
	"php",
	"ruby",
	"sql",
	"toml",
	"tsx",
	"typescript",
	"svelte",
	"vim",
	"vimdoc",
	"yaml",
})

vim.api.nvim_create_autocmd('FileType', {
	callback = function(args)
		-- REPLACES your commented-out large-file `disable` function:
		local max_filesize = 100 * 1024 -- 100 KB
		local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(args.buf))
		if ok and stats and stats.size > max_filesize then
			return
		end
		-- pcall guards filetypes with no installed parser (replaces the old silent no-op)
		pcall(vim.treesitter.start)
	end,
})

require('nvim-treesitter-textobjects').setup({
	move = {
		set_jumps = true, -- CHANGED: `enable = true` is gone on main; `set_jumps` stays
	},
})

local move = require('nvim-treesitter-textobjects.move')

local function map_move(key, fn, query, desc)
	vim.keymap.set({ 'n', 'x', 'o' }, key, function()
		fn(query, 'textobjects')
	end, { desc = desc, silent = true })
end

-- goto_next_start
map_move("]f", move.goto_next_start, "@function.outer", "Next function start")
map_move("]m", move.goto_next_start, "@function.outer", "Next function start")
map_move("]]", move.goto_next_start, "@class.outer", "Next class start")
-- goto_next_end
map_move("]M", move.goto_next_end, "@function.outer", "Next function end")
map_move("]F", move.goto_next_end, "@function.outer", "Next function end")
map_move("][", move.goto_next_end, "@class.outer", "Next class end")
-- goto_previous_start
map_move("[f", move.goto_previous_start, "@function.outer", "Prev function start")
map_move("[m", move.goto_previous_start, "@function.outer", "Prev function start")
map_move("[[", move.goto_previous_start, "@class.outer", "Prev class start")
-- goto_previous_end
map_move("[M", move.goto_previous_end, "@function.outer", "Prev function end")
map_move("[F", move.goto_previous_end, "@function.outer", "Prev function end")
map_move("[]", move.goto_previous_end, "@class.outer", "Prev class end")
