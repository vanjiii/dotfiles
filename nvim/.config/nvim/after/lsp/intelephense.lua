return {
	single_file_support = true,
	settings = {
		intelephense = {
			files = { maxSize = 5 * 1024 * 1024, associations = { "*.php", "*.phtml", "*.inc", "*.tpl" } },
			environment = { phpVersion = "7.3" },
		},
	},
}
