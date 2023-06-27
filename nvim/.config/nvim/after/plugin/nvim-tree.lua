-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup({
	sort_by = "case_sensitive",
	view = {
		width = {
			min = 30,
			max = 50,
		},
	},
	renderer = {
		add_trailing = true,
		highlight_opened_files = "none",
		group_empty = true,
		icons = {
			show = {
				file = false,
				folder = false,
				git = false,
				modified = false,
			},
		},

		indent_markers = {
			enable = true,
			inline_arrows = true,
			icons = {
				corner = "└",
				edge = "│",
				item = "│",
				bottom = "─",
				none = " ",
			},
		},
	},
	filters = {
		dotfiles = false,
		custom = { "^\\.git$" },
	},
	git = {
		enable = false,
	},
	update_focused_file = {
		enable = false
	},
	on_attach = function(bufnr)
		local api = require("nvim-tree.api")
		local opts = function(desc)
			return {
				desc = "nvim-tree: " .. desc,
				buffer = bufnr,
				noremap = true,
				silent = true,
				nowait = true,
			}
		end

		-- open as vsplit on current node
		local function vsplit_preview()
			local node = api.tree.get_node_under_cursor()

			if node.nodes ~= nil then
				-- expand or collapse folder
				api.node.open.edit()
			else
				-- open file as vsplit
				api.node.open.vertical()
			end

			-- Finally refocus on tree if it was lost
			api.tree.focus()
		end

		local function resize()
			print(opt.get_option)
		end

		vim.keymap.set("n", "V", vsplit_preview, opts("Vsplit Preview"))
		vim.keymap.set('n', '<CR>', api.node.open.edit, opts('Open'))
		vim.keymap.set('n', 'C', api.tree.change_root_to_node, opts('CD'))
		vim.keymap.set('n', 'A', resize, opts('CD'))

		-- vim.keymap.set('n', '<leader>F3', open_nvim_tree, opts('Focus opened file'))
	end,
})


vim.api.nvim_set_keymap("n", "<F3>", ":NvimTreeToggle<cr>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<leader><F3>", ":NvimTreeFindFileToggle<cr>", { silent = true, noremap = true })
