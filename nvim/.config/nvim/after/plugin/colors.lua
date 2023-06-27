require('rose-pine').setup({
      --- @usage 'auto'|'main'|'moon'|'dawn'
      variant = 'dawn',
      --- @usage 'main'|'moon'|'dawn'
      dark_variant = 'dawn',
      bold_vert_split = false,
      dim_nc_background = false,
      disable_background = false,
      disable_float_background = false,
      disable_italics = false,
})

vim.cmd('colorscheme rose-pine')
