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
	end,
	settings = {
		Lua = {
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { 'vim' },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
		},
	},
})

-- Configure gopls language server
require('lspconfig').gopls.setup({
	on_attach = function(client, bufnr)
		-- autoformat
		lsp.async_autoformat(client, bufnr)

		local opts = { buffer = bufnr }
		local bind = vim.keymap.set

		bind("n", "<leader>rn", vim.lsp.buf.rename, opts)
		bind("n", "<leader>gh", vim.lsp.buf.document_highlight, opts)
		bind("n", "<leader><F9>", function()
			vim.cmd.noh()
			vim.lsp.buf.clear_references()
		end)
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
