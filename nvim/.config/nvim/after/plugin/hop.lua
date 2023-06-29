require 'hop'.setup {
	case_insensitive = false,
	-- hint_offset = 1,
}

vim.api.nvim_set_keymap("n", "<space><space>", ":HopChar2<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<space>l", ":HopLine<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<space>w", ":HopWord<cr>", { silent = true, noremap = true })
