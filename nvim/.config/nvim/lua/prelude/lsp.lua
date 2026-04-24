--
-- `:LspInfo` - brief info for the current buffer
--
-- `echo executable('gopls')` - return `1` if gopls is found in $PATH
--

vim.lsp.enable({
	'cssls',		-- ✓ from vscode-langservers-extracted
	'gopls',
	'html', 		-- ✓ from vscode-langservers-extracted
	'intelephense',
	'lua_ls',
	'ts_ls',
	-- 'solargraph',
})

vim.api.nvim_create_autocmd('LspAttach', {
	callback = function(args)
		 vim.keymap.set('n', 'gd', vim.lsp.buf.definition,
             { buffer = args.buf, desc = 'vim.lsp.buf.definition()' })

         vim.keymap.set('n', 'gD', vim.lsp.buf.declaration,
             { buffer = args.buf, desc = 'vim.lsp.buf.declaration()' })

         vim.keymap.set('n', '<leader>pl', vim.lsp.buf.format,
             { buffer = args.buf, desc = 'vim.lsp.buf.format()' })

		local client = vim.lsp.get_client_by_id(args.data.client_id)
		print(client.name .. ': Hello there!')
	end,
})

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or "rounded"
	-- vim.print(opts)
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

vim.diagnostic.config({
	float = {
		border = "rounded", -- or "single", "double", "shadow", etc.
	},
})
