require('telescope').setup {
	pickers = {
		find_files = {
			hidden = true
		}
	},
	defaults = {
		file_ignore_patterns = {
			".git"
		},
		mappings = {
			n = {
				-- ['<c-d>'] = require('telescope.actions').delete_buffer
			}, -- n
			i = {
				["<C-h>"] = "which_key",
				['<c-x>'] = require('telescope.actions').delete_buffer,
				['<c-s>'] = require('telescope.actions').select_horizontal,
			} -- i
		} -- mappings
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

vim.keymap.set('n', '<leader>pf', builtin.find_files, opts('find files'))
vim.keymap.set('n', '<C-p>', builtin.find_files, opts('find files'))
vim.keymap.set('n', '<leader>ps', builtin.live_grep, opts('live grep'))
vim.keymap.set('n', '<C-k>', builtin.live_grep, opts('live grep'))
vim.keymap.set('n', '<leader>pg', builtin.git_status, opts('grep git status'))
vim.keymap.set('n', '<leader>pb', builtin.buffers, opts('list buffers'))
vim.keymap.set('n', '<leader>ph', builtin.help_tags, opts('help tags'))
vim.keymap.set("n", "<leader>ss", builtin.lsp_document_symbols, opts('list current buffer symbols'))

---
vim.keymap.set('n', '<leader>pS', builtin.grep_string, opts('grep under cursor'))
vim.keymap.set('n', '<leader>hh', builtin.command_history, opts('history'))
