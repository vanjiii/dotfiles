vim.filetype.add({
  extension = {
    zsh = "sh",  -- Treat .zsh as shell script
  },
  filename = {
    [".zshrc"] = "sh",
    [".zshenv"] = "sh",
    [".zprofile"] = "sh",
  },
})
