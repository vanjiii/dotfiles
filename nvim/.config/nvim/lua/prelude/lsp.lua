--
-- `:LspInfo` - brief info for the current buffer
--
-- `echo executable('gopls')` - return `1` if gopls is found in $PATH
--

vim.lsp.enable({
	'gopls',
	'lua_ls',
	'ts_ls',
	'intelephense',
	-- 'solargraph',
})

vim.api.nvim_create_autocmd('LspAttach', {
	callback = function(args)
		local client = vim.lsp.get_client_by_id(args.data.client_id)
		print(client.name .. ': Hello there!')
	end,
})

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
	opts = opts or {}
	opts.border = opts.border or "rounded"
	vim.print(opts)
	return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

vim.diagnostic.config({
	float = {
		border = "rounded", -- or "single", "double", "shadow", etc.
	},
})
