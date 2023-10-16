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
