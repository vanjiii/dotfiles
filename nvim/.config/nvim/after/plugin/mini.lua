require 'mini.jump2d'.setup {
	allowed_windows = {
		current = true,
		not_current = false,
	},
	view = {
		-- Whether to dim lines with at least one jump spot
		dim = true,

		-- How many steps ahead to show. Set to big number to show all steps.
		n_steps_ahead = 0,
	},
}

require 'mini.surround'.setup {
	mappings = {
		add = 'ys',
		delete = 'ds',
		replace = 'cs',
		find = 'gsf',
		find_left = 'gsF',
		highlight = 'gsh',
		update_n_lines = 'gsn',
	},
}
