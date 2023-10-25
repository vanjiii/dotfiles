-- for some reason `nvim` and `nvim .` render different colors
-- (for examples focus and not-on-focus split are with the same bg).

vim.g.termguicolors = true

local color = require("onenord.colors")

require('onenord').setup({
	theme = "light", -- "dark" or "light". Alternatively, remove the option and set vim.o.background instead
	borders = true, -- Split window borders
	fade_nc = true, -- Fade non-current windows, making them more distinguishable
	-- Style that is applied to various groups: see `highlight-args` for options
	-- styles = {
	-- 	comments = "NONE",
	-- 	strings = "NONE",
	-- 	keywords = "NONE",
	-- 	functions = "NONE",
	-- 	variables = "NONE",
	-- 	diagnostics = "underline",
	-- },
	disable = {
		-- 	background = false, -- Disable setting the background color
		cursorline = false, -- Disable the cursorline
		-- 	eob_lines = true, -- Hide the end-of-buffer lines
	},
	-- Inverse highlight for different groups
	-- inverse = {
	-- 	match_paren = false,
	-- },
	custom_highlights = {
		LspReferenceText = { style = "underline,bold" },
		Visual = { bg = "#EAEBED" },
		VisualNOS = { bg = "#EAEBED" }
	}, -- Overwrite default highlight groups
	-- custom_colors = {},  -- Overwrite default colors
})

vim.cmd('colorscheme onenord-light')
