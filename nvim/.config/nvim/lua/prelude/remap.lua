local opts = function(desc)
	return {
		desc = "Quickfix: " .. desc,
		noremap = true,
		silent = true,
		nowait = true,
	}
end

vim.keymap.set("n", "<F9>", vim.cmd.noh, opts('no highlight [noh]'))

vim.keymap.set("n", "]q", vim.cmd.cn, opts('item next [cn]'))
vim.keymap.set("n", "[q", vim.cmd.cp, opts('item previous [cp]'))
vim.keymap.set("n", "<leader>qc", vim.cmd.cclose, opts('close list [cclose]'))
vim.keymap.set("n", "<leader>qq", vim.cmd.copen, opts('open list [copen]'))

-- Copy relative path
vim.keymap.set('n', '<leader>py', function()
	vim.fn.setreg('+', vim.fn.expand('%')) -- relative path
	vim.notify('Copied relative path to clipboard', vim.log.levels.INFO)
end, opts("Copy relative file path"))

-- Copy absolute path
vim.keymap.set('n', '<leader>pY', function()
	vim.fn.setreg('+', vim.fn.expand('%:p')) -- absolute path
	vim.notify('Copied absolute path to clipboard', vim.log.levels.INFO)
end, opts("Copy absolute file path"))

--
-- stollen from folke
--

-- Move Lines
vim.keymap.set("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move down" })
vim.keymap.set("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move up" })
vim.keymap.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move down" })
vim.keymap.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move up" })
vim.keymap.set("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move down" })
vim.keymap.set("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move up" })
