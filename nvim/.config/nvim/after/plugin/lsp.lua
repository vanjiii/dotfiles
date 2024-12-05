-- Global mappings.
-- :Mason for UI config of the servers.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<leader>d', function()
	vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'diagnostic: ' .. 'go to prev' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'diagnostic: ' .. 'go to next' })
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Reserve a space in the gutter
-- This will avoid an annoying layout shift in the screen
vim.opt.signcolumn = 'yes'

-- https://lsp-zero.netlify.app/v4.x/
local lsp_zero = require('lsp-zero')

local lsp_attach = function(client, bufnr)
	-- see :help lsp-zero-keybindings
	-- to learn the available actions
	lsp_zero.default_keymaps({
		buffer = bufnr,
		-- enforce lsp-zero keymappings
		preserve_mappings = false,
	})
	print(client.name .. ': Hello there.')
end

lsp_zero.extend_lspconfig({
	capabilities = require('cmp_nvim_lsp').default_capabilities(),
	lsp_attach = lsp_attach,
	float_border = 'rounded',
	sign_text = {
		error = '✘',
		warn = '▲',
		hint = '⚑',
		info = '»',
	},
})

require('mason').setup({})
require('mason-lspconfig').setup({
	ensure_installed = { 'lua_ls', 'gopls' }, -- solargraph is omitted
	handlers = {
		-- this first function is the "default handler"
		-- it applies to every language server without a "custom handler"
		function(server_name)
			require('lspconfig')[server_name].setup({})
		end,

		-- {{{
		-- this is the "custom handler" for `lua_ls`
		lua_ls = function()
			require('lspconfig').lua_ls.setup({
				on_init = function(client)
					lsp_zero.nvim_lua_settings(client, {})
				end,
				on_attach = function(client, bufnr)
					-- autoformat
					lsp_zero.async_autoformat(client, bufnr)
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
		end,
		-- }}}

		-- {{{ gopls
		gopls = function()
			require('lspconfig').gopls.setup({
				on_attach = function(client, bufnr)
					-- -- omnifunc
					-- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

					-- autoformat
					lsp_zero.async_autoformat(client, bufnr)

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
						buildFlags = { "-tags=e2e" }
					},
				},
			})
		end,
		-- }}}

		-- {{{ solargraph
		solargraph = function()
			require('lspconfig').solargraph.setup({
				on_attach = function(client, bufnr)
					local opts = { buffer = bufnr }
					local bind = vim.keymap.set

					-- keys
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
				-- hard disabled diagnostics
				-- handlers = {
				-- 	['textDocument/publishDiagnostics'] = function() end
				-- },
			})
		end,
		-- }}}
	}
})

local cmp = require('cmp')

cmp.setup({
	preselect = 'item',
	sources = {
		{ name = 'nvim_lsp' },
	},
	mapping = cmp.mapping.preset.insert({
		-- scroll up and down the documentation window
		['<C-u>'] = cmp.mapping.scroll_docs(-4),
		['<C-d>'] = cmp.mapping.scroll_docs(4),

		['<C-Space>'] = cmp.mapping.complete(),
		['<CR>'] = cmp.mapping.confirm({ select = false }),
	}),
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	completion = {
		autocomplete = false,
		completeopt = 'menu,menuone,noinsert',
	},
})

-- used for autoimports to work
-- https://github.com/golang/tools/blob/master/gopls/doc/vim.md#imports-and-formatting
vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*.go",
	callback = function()
		local params = vim.lsp.util.make_range_params()
		params.context = { only = { "source.organizeImports" } }
		-- buf_request_sync defaults to a 1000ms timeout. Depending on your
		-- machine and codebase, you may want longer. Add an additional
		-- argument after params if you find that you have to write the file
		-- twice for changes to be saved.
		-- E.g., vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
		local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params)
		for cid, res in pairs(result or {}) do
			for _, r in pairs(res.result or {}) do
				if r.edit then
					local enc = (vim.lsp.get_client_by_id(cid) or {}).offset_encoding or "utf-16"
					vim.lsp.util.apply_workspace_edit(r.edit, enc)
				end
			end
		end
		vim.lsp.buf.format({ async = false })
	end
})

-- vim.diagnostic.config({ virtual_text = false }) -- disables those inline suggestions, but does not remove the >> and undertext lines
