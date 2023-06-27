local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
	lsp.default_keymaps({ buffer = bufnr })
	print(client.name .. ': Hello there.')
end)

-- Configure lua language server for neovim
require('lspconfig').lua_ls.setup({
	on_attach = function(client, bufnr)
		-- autoformat
		lsp.async_autoformat(client, bufnr)
	end
})

-- Configure gopls language server
require('lspconfig').gopls.setup({
	on_attach = function(client, bufnr)
		-- autoformat
		lsp.async_autoformat(client, bufnr)
	end,
	settings = {
		gopls = {
			analyses = {
				unusedparams = true,
			},
			staticcheck = true,
		},
	},
})

-- Set goimports on save for go files
vim.api.nvim_create_autocmd('BufWritePre', {
	pattern = '*.go',
	callback = function()
		vim.lsp.buf.code_action({
			context = {
				only = {
					'source.organizeImports'
				}
			},
			apply = true
		})
	end
})

lsp.setup()
