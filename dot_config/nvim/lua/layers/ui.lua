-- UI layer: theme matching Ghostty (Monokai Pro / Source Code Pro for Powerline)
return {
  {
    "loctvl842/monokai-pro.nvim",
    priority = 1000,
    config = function()
      require("monokai-pro").setup({
        filter = "pro", -- matches Ghostty "Monokai Pro" theme
        background_clear = {},
      })
      vim.cmd("colorscheme monokai-pro")
    end,
  },
}
