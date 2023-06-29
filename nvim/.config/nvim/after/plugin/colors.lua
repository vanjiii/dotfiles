-- require('rose-pine').setup({
--     --- @usage 'auto'|'main'|'moon'|'dawn'
--     variant = 'dawn',
--     --- @usage 'main'|'moon'|'dawn'
--     dark_variant = 'dawn',
--     bold_vert_split = false,
--     dim_nc_background = false,
--     disable_background = false,
--     disable_float_background = false,
--     disable_italics = false,
-- })

vim.g.termguicolors = true

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
	-- disable = {
	-- 	background = false, -- Disable setting the background color
	-- 	cursorline = false, -- Disable the cursorline
	-- 	eob_lines = true, -- Hide the end-of-buffer lines
	-- },
	-- Inverse highlight for different groups
	-- inverse = {
	-- 	match_paren = false,
	-- },
	custom_highlights = {
		-- ["@constructor"] = { fg = colors.dark_blue },
		LspReferenceText = { style = "underline,bold" }
	}, -- Overwrite default highlight groups
	-- custom_colors = {},  -- Overwrite default colors
})


vim.cmd('colorscheme onenord-light')
