-- for some reason `nvim` and `nvim .` render different colors
-- (for examples focus and not-on-focus split are with the same bg).

vim.g.termguicolors = true

local background = '#fffff8'
local text = 'text' -- for Tufte css'#111111'

require('rose-pine').setup({
	--- @usage 'auto'|'main'|'moon'|'dawn'
	variant = 'dawn',
	--- @usage 'main'|'moon'|'dawn'
	dark_variant = 'dawn',
	bold_vert_split = false,
	dim_nc_background = false,
	disable_background = false,
	disable_float_background = false,
	disable_italics = true,

	--- @usage string hex value or named color from rosepinetheme.com/palette
	groups = {
		background = background,
		background_nc = '_experimental_nc',
		panel = 'surface',
		panel_nc = 'base',
		border = 'highlight_med',
		comment = 'muted',
		link = 'iris',
		punctuation = 'subtle',

		error = 'love',
		hint = 'iris',
		info = 'foam',
		warn = 'gold',

		headings = {
			h1 = 'iris',
			h2 = 'foam',
			h3 = 'rose',
			h4 = 'gold',
			h5 = 'pine',
			h6 = 'foam',
		}
		-- or set all headings at once
		-- headings = 'subtle'
	},

	-- Change specific vim highlight groups
	-- https://github.com/rose-pine/neovim/wiki/Recipes
	highlight_groups = {
		ColorColumn = { bg = 'rose' },

		-- Blend colours against the "base" background
		CursorLine = { bg = 'foam', blend = 10 },
		StatusLine = { fg = 'highlight_low', bg = 'muted' },
		StatusLineNC = { fg = 'highlight_high', bg = 'overlay' },

		-- Treesitter
		Boolean = { fg = text },
		Function = { fg = text },
		String = { fg = text },
		Constant = { fg = text },
		Number = { fg = text },
		Identitfier = { fg = text },
		Type = { fg = text },
		Label = { fg = text },
		-- Keyword = { bold = true, inherit = true },
		Include = { fg = text }, -- namespace
		['@method'] = { fg = text },
		['@property'] = { fg = text },

		-- language specific shit
		-- ruby
		['@symbol.ruby'] = { fg = text },
		-- lua
		['@constructor.lua'] = { fg = text },

		-- By default each group adds to the existing config.
		-- If you only want to set what is written in this config exactly,
		-- you can set the inherit option:
		-- Search = { bg = 'gold', inherit = false },
	}
})

vim.cmd('colorscheme rose-pine')
