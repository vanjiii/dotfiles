require('telescope').setup {
	pickers = {
		find_files = {
			hidden = true
		}
	},
	defaults = {
		file_ignore_patterns = {
			".git"
		}
	}
}

local builtin = require('telescope.builtin')

local opts = function(desc)
	return {
		desc = "Telescope: " .. desc,
		noremap = true,
		silent = true,
		nowait = true,
	}
end

vim.keymap.set('n', '<leader><leader>', builtin.find_files, opts('find files'))
vim.keymap.set('n', '<leader>pf', builtin.live_grep, opts('live grep'))
vim.keymap.set('n', '<leader>pg', builtin.git_status, opts('grep git status'))
vim.keymap.set('n', '<leader>pb', builtin.buffers, opts('list buffers'))
vim.keymap.set('n', '<leader>ph', builtin.help_tags, opts('help tags'))
vim.keymap.set('n', '<leader>ps', function()
	builtin.grep_string({
		search = vim.fn.input("Grep > "),
		desc = 'Telescope: grep'
	});
end)

---
vim.keymap.set('n', '<leader>pF', builtin.grep_string, opts('grep under cursor'))
vim.keymap.set('n', '<leader>hh', builtin.command_history, opts('history'))
vim.keymap.set('n', '<leader>pl', builtin.quickfix, opts('quickfix list'))
