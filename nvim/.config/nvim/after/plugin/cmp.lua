local cmp = require('cmp')

cmp.setup({
	completion = {
		autocomplete = false, -- Disable auto-popup
	},
	mapping = cmp.mapping.preset.insert({
		['<C-b>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
		['<Tab>'] = cmp.mapping.select_next_item(),
		['<S-Tab>'] = cmp.mapping.select_prev_item(),
		['<C-e>'] = cmp.mapping.abort(),
	}),
	window = {
		-- use the float highlight groups so the popups match lsp.hover
		completion = cmp.config.window.bordered({
			border = 'rounded',
			winhighlight = 'Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None',
		}),
		documentation = cmp.config.window.bordered({
			border = 'rounded',
			winhighlight = 'Normal:NormalFloat,FloatBorder:FloatBorder,Search:None',
		}),
	},
	sources = cmp.config.sources({
		{ name = 'nvim_lsp' }, -- LSP completions
		{ name = 'buffer' }, -- Words from current buffer
		{ name = 'path' }, -- File paths
	})
})
