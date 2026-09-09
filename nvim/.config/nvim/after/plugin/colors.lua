-- for some reason `nvim` and `nvim .` render different colors
-- (for examples focus and not-on-focus split are with the same bg).

vim.o.termguicolors = true
vim.o.background = 'light'

-- zenbones options must be set before :colorscheme
-- see :h zenbones or https://github.com/zenbones-theme/zenbones.nvim
vim.g.zenbones = {
	solid_line_nr = true,
	italic_strings = false,
	lightness = 'bright',
	-- darken_comments = 45,
}

-- zenbones has no option to disable bold keywords; strip it manually
vim.api.nvim_create_autocmd('ColorScheme', {
	pattern = 'zenbones',
	callback = function()
		for _, group in ipairs({ 'Statement' }) do
			local hl = vim.api.nvim_get_hl(0, { name = group, link = false })
			hl.bold = false
			vim.api.nvim_set_hl(0, group, hl)
		end
	end,
})

vim.cmd('colorscheme okcolors-smooth')

require 'vimade'.setup(
	{
		recipe = {
			"minimalist", { animate = true },
		},
		fadelevel = 0.8,
		blocklist = {
			special_buffers = {
				buf_opts = {
					buftype = { "nofile", "quickfix", "terminal", "help", "prompt" },
				},
			},
		},
	}
)
