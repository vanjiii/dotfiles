-- Global mappings.
-- :Mason for UI config of the servers.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
-- TODO dafuq is this shit ?
vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		-- Enable completion triggered by <c-x><c-o>
		vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

		-- Buffer local mappings.
		-- See `:help vim.lsp.*` for documentation on any of the below functions

		local lopts = function(desc)
			return {
				desc = "Global LSP: " .. desc,
				noremap = true,
				silent = true,
				nowait = true,
				buffer = ev.buf
			}
		end
		vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, lopts('declaration'))
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition, lopts('definition'))
		vim.keymap.set('n', '<leader>gd', ':vsplit | lua vim.lsp.buf.definition()<CR>', lopts('definition'))
		vim.keymap.set('n', 'K', vim.lsp.buf.hover, lopts('description'))
		vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, lopts('implementation'))
		vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, lopts('_signature help'))
		vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, lopts('_add work folder'))
		vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, lopts('_remove work folder'))
		vim.keymap.set('n', '<space>wl', function()
			print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
		end, lopts('_list work folders'))
		vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, lopts('type definition'))
		vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, lopts('rename'))
		vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, lopts('code suggestions'))
		vim.keymap.set('n', 'gr', vim.lsp.buf.references, lopts('references'))
		vim.keymap.set('n', '<space>f', function()
			vim.lsp.buf.format { async = true }
		end, lopts('format'))
	end,
})

-- https://github.com/VonHeikemen/lsp-zero.nvim/blob/v3.x/doc/md/api-reference.md
local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(client, bufnr)
	lsp.default_keymaps({ buffer = bufnr })
	print(client.name .. ': Hello there.')
end)

lsp.set_sign_icons({
	error = '✘',
	warn = '▲',
	hint = '⚑',
	info = '»'
})

-- vim.opt.updatetime = 350
-- local lsp_zero = require('lsp-zero')

-- lsp_zero.on_attach(function(client, bufnr)
-- 	lsp_zero.highlight_symbol(client, bufnr)
-- end)

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

-- ruby lsp server
require('lspconfig').solargraph.setup({
	on_attach = function(client, bufnr)
		-- autoformat
		-- lsp.async_autoformat(client, bufnr)

		local opts = { buffer = bufnr }
		local bind = vim.keymap.set

		-- keys
		-- bind("n", "<leader>gf", function()
		-- 	lsp.async_autoformat(client, bufnr)
		-- end
		-- , opts)
		bind("n", "<leader>gh", vim.lsp.buf.document_highlight, opts)
		bind("n", "<leader><F9>", function()
			vim.cmd.noh()
			vim.lsp.buf.clear_references()
		end)
	end,
	cmd = {
		os.getenv("HOME") .. "/bin/solargraph", 'stdio'
	},
	settings = {
		solargraph = {
			autoformat = false,
			formatting = false,
			completion = true,
			diagnostic = true,
			folding = true,
			references = true,
			rename = true,
			symbols = true
		}
	},
	init_options = {
		formatting = true,
	},
})

-- Configure gopls language server
require('lspconfig').gopls.setup({
	on_attach = function(client, bufnr)
		-- autoformat
		lsp.async_autoformat(client, bufnr)

		local opts = { buffer = bufnr }
		local bind = vim.keymap.set

		-- keys
		-- TODO make this global
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
-- TODO make this via lsp
-- https://github.com/golang/tools/blob/master/gopls/doc/vim.md#neovim-imports
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
