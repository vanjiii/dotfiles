return {
	cmd       = { 'mise', 'x', '--', 'lua-language-server' },
	on_attach = function(client, bufnr)
		if not client.supports_method('textDocument/formatting') then
			print('lsp server does not support autoformat...')
			return
		end

		local group = vim.api.nvim_create_augroup('LspAutoFormat_' .. bufnr, { clear = false })
		vim.api.nvim_create_autocmd('BufWritePre', {
			group = group,
			buffer = bufnr,
			callback = function()
				vim.lsp.buf.format({ bufnr = bufnr, id = client.id })
			end,
		})
	end,
}
