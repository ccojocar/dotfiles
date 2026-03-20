-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Layers mirror SpaceVim's layer philosophy:
--   ui     → appearance (theme)
--   editor → editing tools (treesitter, telescope)
--   lsp    → language server protocol + completion + formatting
--   lang   → language-specific enhancements (Rust, Go)
require("lazy").setup({
  { import = "layers.ui" },
  { import = "layers.editor" },
  { import = "layers.lsp" },
  { import = "layers.lang" },
}, {
  change_detection = { notify = false },
})
