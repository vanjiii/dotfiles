local function start_golangcilint()
	if vim.bo.filetype ~= 'go' then
		vim.notify('Not a Go buffer', vim.log.levels.WARN)
		return
	end
	vim.lsp.start({
		name = 'golangcilint',
		cmd = { 'golangci-lint-langserver' },
		root_dir = vim.fs.root(0, { 'go.mod', '.git' }),
		init_options = {
			command = {
				'golangci-lint', 'run',
				'--output.json.path=stdout',
				'--show-stats=false',
				'--issues-exit-code=1',
			},
		},
	})
end

local function stop_golangcilint()
	for _, c in ipairs(vim.lsp.get_clients({ name = 'golangcilint', bufnr = 0 })) do
		vim.lsp.stop_client(c.id)
	end
end

vim.keymap.set('n', '<leader>ll', start_golangcilint, { desc = 'golangci-lint: start on buffer' })
vim.keymap.set('n', '<leader>lc', stop_golangcilint, { desc = 'golangci-lint: stop' })
