local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup(
-- Plugins
	{
		{ 'rose-pine/neovim',                           name = 'rose-pine', lazy = false, priority = 53 },
		-- { "catppuccin/nvim",                 name = "catppuccin", priority = 52 },
		-- { 'rmehri01/onenord.nvim',           lazy = false,        priority = 51, },

		{ 'nvim-treesitter/nvim-treesitter',            build = ':TSUpdate' },
		{ 'nvim-treesitter/nvim-treesitter-textobjects' },

		{
			'nvim-telescope/telescope.nvim',
			tag = '0.1.4',
			dependencies = { 'nvim-lua/plenary.nvim' }
		},

		-- {{{ lsp-zero
		-- TODO: remove lsp-zero dep
		{ 'VonHeikemen/lsp-zero.nvim', branch = 'v4.x', },
		{ 'neovim/nvim-lspconfig' },
		{ 'hrsh7th/cmp-nvim-lsp' },
		{ 'hrsh7th/nvim-cmp' },
		{
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
		},
		-- }}}

		{ 'nvim-tree/nvim-tree.lua' },

		-- TODO: smoka7/hop.nvim move to this or other like from mini.nvim or folke's QoL
		-- maybe remove it at all, after all there is
		-- realtive numbers; f, F; search
		{ 'phaazon/hop.nvim',       branch = 'v2' },

		{
			'folke/which-key.nvim',
			event = "VeryLazy",
			init = function()
				vim.o.timeout = true
				vim.o.timeoutlen = 500
			end,
			opts = {
				plugins = {
					marks = false,
					registers = false,
				},
			}
		},

		{
			'lewis6991/gitsigns.nvim',
			config = function()
				require('gitsigns').setup()
			end,
		},

		{
			'cappyzawa/trim.nvim',
			opts = {}
		},
		{
			-- Detect tabstop and shiftwidth automatically
			'tpope/vim-sleuth',
		},
	},
	--
	-- Lazy options
	--
	{
		root = vim.fn.stdpath("data") .. "/lazy", -- directory where plugins will be installed
		defaults = {
			lazy = false,                   -- should plugins be lazy-loaded?
			version = nil,
			-- default `cond` you can use to globally disable a lot of plugins
			-- when running inside vscode for example
			cond = nil, ---@type boolean|fun(self:LazyPlugin):boolean|nil
			-- version = "*", -- enable this to try installing the latest stable versions of plugins
		},
		-- leave nil when passing the spec as the first argument to setup()
		spec = nil, ---@type LazySpec
		lockfile = vim.fn.stdpath("config") .. "/lazy-lock.json", -- lockfile generated after running update.
		concurrency = nil, ---@type number limit the maximum amount of concurrent tasks
		git = {
			-- defaults for the `Lazy log` command
			-- log = { "-10" }, -- show the last 10 commits
			log = { "-8" }, -- show commits from the last 3 days
			timeout = 120, -- kill processes that take more than 2 minutes
			url_format = "https://github.com/%s.git",
			-- lazy.nvim requires git >=2.19.0. If you really want to use lazy with an older version,
			-- then set the below to false. This should work, but is NOT supported and will
			-- increase downloads a lot.
			filter = true,
		},
		dev = {
			-- directory where you store your local plugin projects
			path = "~/Projects",
			---@type string[] plugins that match these patterns will use your local versions instead of being fetched from GitHub
			patterns = {}, -- For example {"folke"}
			fallback = false, -- Fallback to git when local plugin doesn't exist
		},
		install = {
			-- install missing plugins on startup. This doesn't increase startup time.
			missing = true,
			-- try to load one of these colorschemes when starting an installation during startup
			colorscheme = { "habamax" },
		},
		ui = {
			-- a number <1 is a percentage., >1 is a fixed size
			size = { width = 0.8, height = 0.8 },
			wrap = true, -- wrap the lines in the ui
			-- The border to use for the UI window. Accepts same border values as |nvim_open_win()|.
			border = "none",
			title = nil, ---@type string only works when border is not "none"
			title_pos = "center", ---@type "center" | "left" | "right"
			icons = {
				cmd = "⌘",
				config = "🛠",
				event = "📅",
				ft = "📂",
				init = "⚙",
				keys = "🗝",
				loaded = "●",
				not_loaded = "○",
				plugin = "🔌",
				runtime = "💻",
				source = "📄",
				start = "🚀",
				task = "📌",
				lazy = "💤 ",
				list = {
					"●",
					"➜",
					"★",
					"‒",
				},
			},
			-- leave nil, to automatically select a browser depending on your OS.
			-- If you want to use a specific browser, you can define it here
			browser = nil, ---@type string?
			throttle = 20, -- how frequently should the ui process render events
			custom_keys = {
				-- you can define custom key maps here.
				-- To disable one of the defaults, set it to false

				-- open lazygit log
				["<localleader>l"] = function(plugin)
					require("lazy.util").float_term({ "lazygit", "log" }, {
						cwd = plugin.dir,
					})
				end,

				-- open a terminal for the plugin dir
				["<localleader>t"] = function(plugin)
					require("lazy.util").float_term(nil, {
						cwd = plugin.dir,
					})
				end,
			},
		},
		diff = {
			-- diff command <d> can be one of:
			-- * browser: opens the github compare view. Note that this is always mapped to <K> as well,
			--   so you can have a different command for diff <d>
			-- * git: will run git diff and open a buffer with filetype git
			-- * terminal_git: will open a pseudo terminal with git diff
			-- * diffview.nvim: will open Diffview to show the diff
			cmd = "git",
		},
		checker = {
			-- automatically check for plugin updates
			enabled = false,
			concurrency = nil, ---@type number? set to 1 to check for updates very slowly
			notify = true,       -- get a notification when new updates are found
			frequency = 60 * 60 * 24 * 7, -- check for updates weekly
		},
		change_detection = {
			-- automatically check for config file changes and reload the ui
			enabled = true,
			notify = true, -- get a notification when changes are found
		},
		performance = {
			cache = {
				enabled = true,
			},
			reset_packpath = true, -- reset the package path to improve startup time
			rtp = {
				reset = true, -- reset the runtime path to $VIMRUNTIME and your config directory
				---@type string[]
				paths = {}, -- add any custom paths here that you want to includes in the rtp
				---@type string[] list any plugins you want to disable here
				disabled_plugins = {
					-- "gzip",
					-- "matchit",
					-- "matchparen",
					-- "netrwPlugin",
					-- "tarPlugin",
					-- "tohtml",
					-- "tutor",
					-- "zipPlugin",
				},
			},
		},
		-- lazy can generate helptags from the headings in markdown readme files,
		-- so :help works even for plugins that don't have vim docs.
		-- when the readme opens with :help it will be correctly displayed as markdown
		readme = {
			enabled = true,
			root = vim.fn.stdpath("state") .. "/lazy/readme",
			files = { "README.md", "lua/**/README.md" },
			-- only generate markdown helptags for plugins that dont have docs
			skip_if_doc_exists = true,
		},
		state = vim.fn.stdpath("state") .. "/lazy/state.json", -- state info for checker and other things
	})
